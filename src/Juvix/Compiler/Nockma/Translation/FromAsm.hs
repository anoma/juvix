module Juvix.Compiler.Nockma.Translation.FromAsm where

import Juvix.Prelude hiding (Atom, Path)
import Juvix.Compiler.Asm.Data.InfoTable qualified as Asm
-- import Juvix.Compiler.Asm.Language qualified as Asm
import Juvix.Compiler.Nockma.Pretty
import Juvix.Compiler.Nockma.Stdlib
import Juvix.Compiler.Nockma.Evaluator.Error
import Juvix.Compiler.Nockma.Evaluator


fromAsm :: Members '[] r => Asm.Symbol -> Asm.InfoTable -> Sem r (Term Natural)
fromAsm mainfun tab = undefined

data StackName
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
stdlibPath = decodePath' . EncodedPath . \case
  StdlibDec -> 342
  StdlibAdd -> 20

numStacks :: (Integral a) => a
numStacks = fromIntegral (length (allElements @StackName))

data Compiler m a where
  Push :: StackName -> Term Natural -> Compiler m ()
  Pop :: StackName -> Compiler m ()
  Call :: Text -> Natural -> Compiler m ()
  Increment :: StackName -> Compiler m ()
  Branch :: m () -> m () -> Compiler m ()
  CallStdlib :: StdlibFunction -> Compiler m ()
  AsmReturn :: Compiler m ()

-- | The path to the head of a named stack
stackPath :: StackName -> Path
stackPath s = indexStack (fromIntegral (fromEnum s))

indexStack :: Natural -> Path
indexStack idx = replicate idx R ++ [L]

indexInPath :: Path -> Natural -> Path
indexInPath p idx = p ++ indexStack idx

indexInStack :: StackName -> Natural -> Path
indexInStack s idx = stackPath s ++ indexStack idx

pathToArgumentsArea :: Path
pathToArgumentsArea = indexInStack CurrentFunction 0 ++ [R]

pathToArg :: Natural -> Path
pathToArg = indexInPath pathToArgumentsArea

-- | Construct a path rooted at he head of a named stack
pathInStack :: StackName -> Path -> Path
pathInStack s p = stackPath s ++ p

data StackId
  = StackMemory
  | StackMain
  | StackProgram
  deriving stock (Enum, Bounded)

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
    initSubStack :: StackName -> Term Natural
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

runCompiledNock :: (MonadIO m) => Sem '[Compiler, Reader (HashMap Text Path)] () -> m ()
runCompiledNock s = do
  let t =
        run
          . runReader functionLocations
          . execCompiler
          $ s
      stack = initStack functions
      evalT =
        run
          . runError @(ErrNockNatural Natural)
          . runError @NockEvalError
          $ eval stack t
  case evalT of
    Left e -> error (show e)
    Right ev -> case ev of
      Left e -> error (show e)
      Right res -> putStrLn (ppTrace res)

execCompiler :: (Member (Reader (HashMap Text Path)) r) => Sem (Compiler ': r) a -> Sem r (Term Natural)
execCompiler = fmap fst . runCompiler

runCompiler :: (Member (Reader (HashMap Text Path)) r) => Sem (Compiler ': r) a -> Sem r (Term Natural, a)
runCompiler sem = do
  (ts, a) <- runOutputList (re sem)
  return (seqTerms ts, a)

re :: (Member (Reader (HashMap Text Path)) r) => Sem (Compiler ': r) a -> Sem (Output (Term Natural) ': r) a
re = reinterpretH $ \case
  Push s n -> outputT (pushOnStack s n)
  Pop s -> outputT (popStack 1 s)
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
    output (pushOnStack CurrentFunction
              (OpReplace #
                   -- TODO: copy funArgs elements from the value stack
                   (([R] # stackTake ValueStack funArgsNum) #
                    OpAddress # funPath)))

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
        extractResult = (OpAddress # [L]) # (OpAddress # [R,R])
        callFn = OpPush # (OpCall # [L] # (OpReplace # ([R, L] # arguments) # (OpAddress # [L]))) # extractResult

    output (OpPush # decodeFn # callFn)
    output (replaceTopStack ValueStack)
    where
      stdlibStackTake :: StackName -> Natural -> Term Natural
      stdlibStackTake sn n = foldr1 (#) (take (fromIntegral n) [OpAddress # indexInStack sn i | i <- [0..]])

  AsmReturn -> (pureT =<<) $ do
    output (replaceStack ValueStack ((OpAddress # indexInStack ValueStack 0) # (OpAddress # indexInStack CallStack 0)))
    -- discard the 'activation' frame
    output (popStack 1 CallStack)
    output (popStack 1 CurrentFunction)

  where
    outputT :: (Functor f, Member (Output (Term Natural)) r) => Term Natural -> Sem (WithTactics e f m r) (f ())
    outputT = pureT <=< output

stackTake :: StackName -> Natural -> Term Natural
stackTake sn n = remakeList (take (fromIntegral n) [OpAddress # indexInStack sn i | i <- [0..]])

pushOnStack :: StackName -> Term Natural -> Term Natural
pushOnStack s t = OpPush # t # topStack s

replaceOnStack :: StackName -> Term Natural -> Term Natural
replaceOnStack s t = OpPush # t # replaceTopStack s

popStack :: Natural -> StackName -> Term Natural
popStack n sn =
  remakeList
    [ let p = stackPath s
          a
            | sn == s = p ++ replicate n R
            | otherwise = p
        in OpAddress # a
      | s <- allElements
    ]

replaceStack :: StackName -> Term Natural -> Term Natural
replaceStack sn t =
  remakeList
    [ if
            | sn == s -> t
            | otherwise -> OpAddress # (stackPath s)
      | s <- allElements
    ]

resetStack :: StackName -> Term Natural
resetStack sn = replaceStack sn (OpQuote # nockNil')

-- | Reconstruct the value-stack / call-stack cell by moving the global head to the
-- respective stack head.
--- [h [s1 s1 s3 nil]]
--- [ s1 .. [h si] ... sn nil]
topStack :: StackName -> Term Natural
topStack sn =
  remakeList
    [ let p = OpAddress # (R : stackPath s)
        in if
            | sn == s -> (OpAddress # [L]) # p
            | otherwise -> p
      | s <- allElements
    ]

replaceTopStack :: StackName -> Term Natural
replaceTopStack sn =
  remakeList
    [ let p = R : stackPath s
        in if
            | sn == s -> (OpAddress # [L]) # (OpAddress # (p ++ [R]))
            | otherwise -> OpAddress # p
      | s <- allElements
    ]

pushNat :: (Member Compiler r) => StackName -> Natural -> Sem r ()
pushNat s n = push s (OpQuote # toNock n)

prog1 :: Sem '[Compiler, Reader (HashMap Text Path)] ()
prog1 = do
  pushNat ValueStack 2
  pushNat ValueStack 3
  increment ValueStack
  -- callStdlib StdlibDec
  -- callStdlib StdlibAdd
  call "increment" 1
  -- call "increment" 1
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
