module Juvix.Compiler.Nockma.Translation.FromAsm where

import Juvix.Compiler.Asm.Data.InfoTable qualified as Asm
-- import Juvix.Compiler.Asm.Language qualified as Asm

import Juvix.Compiler.Nockma.Evaluator
import Juvix.Compiler.Nockma.Evaluator.Error
import Juvix.Compiler.Nockma.Pretty
import Juvix.Compiler.Nockma.Stdlib
import Juvix.Prelude hiding (Atom, Path)

type NockmaCompiler r = Sem (Compiler ': Reader (HashMap Text Path) ': r) ()

fromAsm :: Asm.Symbol -> Asm.InfoTable -> Term Natural
fromAsm = undefined

data StackId
  = CurrentFunction
  | ValueStack
  | CallStack
  | TempStack
  | FunctionsLibrary
  | StandardLibrary
  deriving stock (Enum, Bounded, Eq, Show)

data StdlibFunction
  = StdlibDec
  | StdlibAdd

stdlibNumArgs :: StdlibFunction -> Natural
stdlibNumArgs = \case
  StdlibDec -> 1
  StdlibAdd -> 2

stdlibPath :: StdlibFunction -> Path
stdlibPath =
  decodePath' . EncodedPath . \case
    StdlibDec -> 342
    StdlibAdd -> 20

numStacks :: (Integral a) => a
numStacks = fromIntegral (length (allElements @StackId))

data Compiler m a where
  Verbatim :: Term Natural -> Compiler m ()
  Push :: StackId -> Term Natural -> Compiler m ()
  Pop :: StackId -> Compiler m ()
  Call :: Text -> Natural -> Compiler m ()
  Increment :: StackId -> Compiler m ()
  Branch :: m () -> m () -> Compiler m ()
  CallStdlib :: StdlibFunction -> Compiler m ()
  AsmReturn :: Compiler m ()

stackPath :: StackId -> Path
stackPath s = indexStack (fromIntegral (fromEnum s))

indexStack :: Natural -> Path
indexStack idx = replicate idx R ++ [L]

indexInPath :: Path -> Natural -> Path
indexInPath p idx = p ++ indexStack idx

indexInStack :: StackId -> Natural -> Path
indexInStack s idx = stackPath s ++ indexStack idx

pathToArgumentsArea :: Path
pathToArgumentsArea = indexInStack CurrentFunction 0 ++ [R]

pathToArg :: Natural -> Path
pathToArg = indexInPath pathToArgumentsArea

-- | Construct a path rooted at he head of a named stack
pathInStack :: StackId -> Path -> Path
pathInStack s p = stackPath s ++ p

makeSem ''Compiler

seqTerms :: [Term Natural] -> Term Natural
seqTerms = foldl' step (OpAddress # emptyPath) . reverse
  where
    step :: Term Natural -> Term Natural -> Term Natural
    step acc t = OpSequence # t # acc

makeList :: [Term Natural] -> Term Natural
makeList ts = foldr1 (#) (ts ++ [TermAtom nockNil])

remakeList :: [Term Natural] -> Term Natural
remakeList ts = foldr1 (#) (ts ++ [OpQuote # nockNil'])

nockNil' :: Term Natural
nockNil' = TermAtom nockNil

initStack :: [Term Natural] -> Term Natural
initStack defs = makeList (initSubStack <$> allElements)
  where
    initSubStack :: StackId -> Term Natural
    initSubStack = \case
      CurrentFunction -> nockNil'
      ValueStack -> nockNil'
      CallStack -> nockNil'
      TempStack -> nockNil'
      StandardLibrary -> stdlib
      FunctionsLibrary -> makeList defs

compileFunctions :: [(Text, Sem '[Compiler, Reader (HashMap Text Path)] ())]
compileFunctions = [("increment", compileFunIncrement), ("const", compileFunConst), ("callInc", compileCallInc)]

functions :: [Term Natural]
functions = (# nockNil') <$> run (runReader functionLocations (mapM execCompiler (snd <$> compileFunctions)))

functionLocations :: HashMap Text Path
functionLocations =
  hashMap
    [ (n, indexInStack FunctionsLibrary i)
      | (i, (n, _)) <- zip [0 ..] compileFunctions
    ]

funIncrement' :: Term Natural
funIncrement' = funCode # nockNil'
  where
    funCode :: Term Natural
    funCode = OpPush # (OpInc # (OpAddress # pathToArg 0)) # topStack ValueStack

compileFunIncrement :: Sem '[Compiler, Reader (HashMap Text Path)] ()
compileFunIncrement = do
  push ValueStack (OpInc # (OpAddress # pathToArg 0))
  asmReturn

funConst :: Term Natural
funConst = funCode # nockNil'
  where
    funCode :: Term Natural
    funCode = OpPush # (OpAddress # pathToArg 0) # topStack ValueStack

compileFunConst :: Sem '[Compiler, Reader (HashMap Text Path)] ()
compileFunConst = do
  push ValueStack (OpAddress # pathToArg 0)
  asmReturn

compileCallInc :: Sem '[Compiler, Reader (HashMap Text Path)] ()
compileCallInc = do
  push ValueStack (OpAddress # pathToArg 0)
  call "increment" 1
  asmReturn

execCompiler :: (Member (Reader (HashMap Text Path)) r) => Sem (Compiler ': r) a -> Sem r (Term Natural)
execCompiler = fmap fst . runCompiler

runCompiler :: (Member (Reader (HashMap Text Path)) r) => Sem (Compiler ': r) a -> Sem r (Term Natural, a)
runCompiler sem = do
  (ts, a) <- runOutputList (re sem)
  return (seqTerms ts, a)

re :: (Member (Reader (HashMap Text Path)) r) => Sem (Compiler ': r) a -> Sem (Output (Term Natural) ': r) a
re = reinterpretH $ \case
  Push s n -> outputT (pushOnStack s n)
  Pop s -> outputT (popStack s)
  Verbatim s -> outputT s
  Call funName funArgsNum -> (pureT =<<) $ do
    -- 1. Obtain the path to the function
    -- 2.
    --   i. Take a copy of the value stack without the arguments to the function
    --   ii. Push this copy to the call stack
    -- 3.
    --  i. Take a copy of the function from the function library
    --  ii. Replace its argument area with the arguments from the value stack
    --  iii. Push this copy to the current function stack
    -- 4. Replace the value stack with nil
    -- 5. Evaluate the function in the context of the whole nock stack
    --    The return instruction in the function compiles to:
    --      i. Restore the previous value stack (created in 2.). i.e copy the previous value stack
    --         from the call stack and push the result (the head of the current value stack) to it.
    --      i. Pop the call stack and the current function stack.

    funPath <- fromJust <$> asks @(HashMap Text Path) (^. at funName)

    -- Save the current state of the value stack
    output (pushOnStack CallStack (OpAddress # pathInStack ValueStack (replicate funArgsNum R)))

    -- Setup function to call with it arguments
    output
      ( pushOnStack
          CurrentFunction
          ( OpReplace
              #
              -- TODO: copy funArgs elements from the value stack
              ( ([R] # stackTake ValueStack funArgsNum)
                  # OpAddress
                  # funPath
              )
          )
      )

    -- Init 'activation frame'
    output (resetStack ValueStack)

    -- call the function
    output (OpCall # ((indexInStack CurrentFunction 0 ++ [L]) # (OpAddress # emptyPath)))
  Increment s -> (pureT =<<) $ do
    output
      ( replaceOnStack
          s
          (OpInc # (OpAddress # pathInStack s [L]))
      )
  Branch t f -> do
    termT <- runT t >>= raise . execCompiler . (pop ValueStack >>)
    termF <- runT f >>= raise . execCompiler . (pop ValueStack >>)
    outputT (OpIf # (OpAddress # pathInStack ValueStack [L]) # termT # termF)
  CallStdlib f -> (pureT =<<) $ do
    let fNumArgs = stdlibNumArgs f
        fPath = stdlibPath f
        decodeFn = OpCall # (fPath # (OpAddress # stackPath StandardLibrary))
        arguments = OpSequence # (OpAddress # [R]) # stdlibStackTake ValueStack fNumArgs
        extractResult = (OpAddress # [L]) # (OpAddress # [R, R])
        callFn = OpPush # (OpCall # [L] # (OpReplace # ([R, L] # arguments) # (OpAddress # [L]))) # extractResult

    output (OpPush # decodeFn # callFn)
    output (replaceTopStack ValueStack)
    where
      stdlibStackTake :: StackId -> Natural -> Term Natural
      stdlibStackTake sn n = foldr1 (#) (take (fromIntegral n) [OpAddress # indexInStack sn i | i <- [0 ..]])
  AsmReturn -> (pureT =<<) $ do
    output (replaceStack ValueStack ((OpAddress # indexInStack ValueStack 0) # (OpAddress # indexInStack CallStack 0)))
    -- discard the 'activation' frame
    output (popStack CallStack)
    output (popStack CurrentFunction)
  where
    outputT :: (Functor f, Member (Output (Term Natural)) r) => Term Natural -> Sem (WithTactics e f m r) (f ())
    outputT = pureT <=< output

stackTake :: StackId -> Natural -> Term Natural
stackTake sn n = remakeList (take (fromIntegral n) [OpAddress # indexInStack sn i | i <- [0 ..]])

pushOnStack :: StackId -> Term Natural -> Term Natural
pushOnStack s t = OpPush # t # topStack s

replaceOnStack :: StackId -> Term Natural -> Term Natural
replaceOnStack s t = OpPush # t # replaceTopStack s

popStack :: StackId -> Term Natural
popStack = popStackN 1

popStackN :: Natural -> StackId -> Term Natural
popStackN n sn =
  remakeList
    [ let p = stackPath s
          a
            | sn == s = p ++ replicate n R
            | otherwise = p
       in OpAddress # a
      | s <- allElements
    ]

replaceStack :: StackId -> Term Natural -> Term Natural
replaceStack sn t =
  remakeList
    [ if
          | sn == s -> t
          | otherwise -> OpAddress # (stackPath s)
      | s <- allElements
    ]

resetStack :: StackId -> Term Natural
resetStack sn = replaceStack sn (OpQuote # nockNil')

-- | Reconstruct the value-stack / call-stack cell by moving the global head to the
-- respective stack head.

--- [h [s1 s1 s3 nil]]
--- [ s1 .. [h si] ... sn nil]
topStack :: StackId -> Term Natural
topStack sn =
  remakeList
    [ let p = OpAddress # (R : stackPath s)
       in if
              | sn == s -> (OpAddress # [L]) # p
              | otherwise -> p
      | s <- allElements
    ]

replaceTopStack :: StackId -> Term Natural
replaceTopStack sn =
  remakeList
    [ let p = R : stackPath s
       in if
              | sn == s -> (OpAddress # [L]) # (OpAddress # (p ++ [R]))
              | otherwise -> OpAddress # p
      | s <- allElements
    ]

pushNat :: (Member Compiler r) => StackId -> Natural -> Sem r ()
pushNat s n = push s (OpQuote # toNock n)

debugProgPrint :: NockmaCompiler '[Embed IO] -> IO ()
debugProgPrint = debugProg >=> putStrLn . ppTrace

debugProg :: NockmaCompiler '[Embed IO] -> IO (Term Natural)
debugProg m = runM $ runCompiledNock takeValueStack
  where
    takeValueStack :: NockmaCompiler '[Embed IO]
    takeValueStack = do
      m
      verbatim (OpAddress # stackPath ValueStack)

runCompiledNock :: (Members '[Embed IO] r) => Sem (Compiler ': Reader (HashMap Text Path) ': r) () -> Sem r (Term Natural)
runCompiledNock s = do
  let stack = initStack functions
  t <-
    runReader functionLocations
      . execCompiler
      $ s
  evalT <-
    runError @(ErrNockNatural Natural)
      . runError @NockEvalError
      $ eval stack t
  case evalT of
    Left e -> error (show e)
    Right ev -> case ev of
      Left e -> error (show e)
      Right res -> return res

prog1 :: (Members '[Compiler, Reader (HashMap Text Path)] r) => Sem r ()
prog1 = do
  pushNat ValueStack 2
  pushNat ValueStack 3
  increment ValueStack
  -- callStdlib StdlibDec
  callStdlib StdlibAdd
  call "increment" 1
  -- call "increment" 1
  pushNat ValueStack 8
  call "const" 2
  call "callInc" 1

-- pushNat ValueStack 111
-- pushNat ValueStack 222
-- pushNat ValueStack 333
-- call "increment" 1
-- pushNat ValueStack 444
-- call "increment" 1
-- push FunctionsLibrary funIncrement
-- pushNat ValueStack 13
-- pushNat TempStack 20
-- pushNat CallStack 666
-- increment CallStack
-- pushNat ValueStack 13
-- increment CallStack
-- pop ValueStack
-- pushNat ValueStack 50
-- pushNat CallStack 33
-- pushNat CallStack 44
-- pop CallStack
-- pop ValueStack
-- increment CallStack
-- pushNat ValueStack 0
-- branch (pushNat ValueStack 99) (pushNat ValueStack 77)
