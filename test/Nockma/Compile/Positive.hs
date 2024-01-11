{-# LANGUAGE QuasiQuotes #-}

module Nockma.Compile.Positive where

import Base hiding (Path)
import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Compiler.Asm.Language qualified as Asm
import Juvix.Compiler.Nockma.Evaluator
import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Pretty
import Juvix.Compiler.Nockma.Translation.FromAsm
import Juvix.Compiler.Nockma.Translation.FromSource.QQ

type Check = Sem '[Reader [Term Natural], Reader (Term Natural), Embed IO]

data Test = Test
  { _testName :: Text,
    _testCheck :: Check (),
    _testProgram :: Sem '[Compiler] ()
  }

makeLenses ''Test

data FunctionName
  = FunMain
  | FunIncrement
  | FunConst
  | FunConst5
  | FunCallInc
  | FunAdd3
  deriving stock (Eq, Bounded, Enum)

sym :: (Enum a) => a -> FunctionId
sym = UserFunction . Asm.defaultSymbol . fromIntegral . fromEnum

debugProg :: Sem '[Compiler] () -> ([Term Natural], Term Natural)
debugProg mkMain = run . runOutputList $ compileAndRunNock' opts exampleConstructors exampleFunctions mainFun
  where
    mainFun =
      CompilerFunction
        { _compilerFunctionName = sym FunMain,
          _compilerFunctionArity = 0,
          _compilerFunction = raiseUnder mkMain
        }

    opts = CompilerOptions {_compilerOptionsEnableTrace = True}

isMain :: FunctionName -> Bool
isMain = (== FunMain)

functionArity' :: FunctionName -> Natural
functionArity' = \case
  FunMain -> 0
  FunIncrement -> 1
  FunConst -> 2
  FunCallInc -> 1
  FunConst5 -> 5
  FunAdd3 -> 3

functionCode :: (Members '[Compiler] r) => FunctionName -> Sem r ()
functionCode = \case
  FunMain -> impossible
  FunIncrement -> do
    push (OpInc # (OpAddress # pathToArg 0))
    asmReturn
  FunConst5 -> do
    push (OpAddress # pathToArg 0)
    asmReturn
  FunConst -> do
    push (OpAddress # pathToArg 0)
    asmReturn
  FunCallInc -> do
    push (OpAddress # pathToArg 0)
    callFun (sym FunIncrement) 1
    asmReturn
  FunAdd3 -> do
    push (OpAddress # pathToArg 0)
    push (OpAddress # pathToArg 1)
    push (OpAddress # pathToArg 2)
    add
    add
    asmReturn

-- | NOTE that new constructors should be added to the end of the list or else
-- the tests will fail.
data ConstructorName
  = ConstructorFalse
  | ConstructorTrue
  | ConstructorWrapper
  | ConstructorPair
  deriving stock (Eq, Bounded, Enum)

constructorTag :: ConstructorName -> Asm.Tag
constructorTag n = Asm.UserTag (Asm.TagUser defaultModuleId (fromIntegral (fromEnum n)))

constructorArity :: ConstructorName -> Natural
constructorArity = \case
  ConstructorFalse -> 0
  ConstructorTrue -> 0
  ConstructorWrapper -> 1
  ConstructorPair -> 2

exampleConstructors :: ConstructorArities
exampleConstructors =
  hashMap $
    [ (constructorTag n, constructorArity n)
      | n <- allElements
    ]
      ++ [ (Asm.BuiltinTag Asm.TagTrue, 0),
           (Asm.BuiltinTag Asm.TagFalse, 0)
         ]

exampleFunctions :: [CompilerFunction]
exampleFunctions =
  [ CompilerFunction (sym fun) (functionArity' fun) (functionCode fun)
    | fun <- allElements,
      not (isMain fun)
  ]

allTests :: TestTree
allTests = testGroup "Nockma compile unit positive" (map mk tests)
  where
    mk :: Test -> TestTree
    mk Test {..} = testCase (unpack _testName) $ do
      let (traces, n) = debugProg _testProgram
      runM (runReader n (runReader traces _testCheck))

eqSubStack :: StackId -> Path -> Term Natural -> Check ()
eqSubStack st subp expected = subStackPred st subp $
  \n -> unless (n == expected) (err n)
  where
    err :: Term Natural -> Check ()
    err n = do
      let msg =
            "Expected "
              <> show st
              <> ":\n"
              <> ppTrace expected
              <> "\nBut got:\n"
              <> ppTrace n
      assertFailure (unpack msg)

eqTraces :: [Term Natural] -> Check ()
eqTraces expected = do
  ts <- ask
  unless (ts == expected) (err ts)
  where
    err :: [Term Natural] -> Check ()
    err ts = do
      let msg =
            "Expected traces:\n"
              <> ppTrace expected
              <> "\nBut got:\n"
              <> ppTrace ts
      assertFailure (unpack msg)

subStackPred :: StackId -> Path -> (Term Natural -> Check ()) -> Check ()
subStackPred st subp p = do
  s <- getStack st <$> ask
  case run (runError @NockEvalError (subTerm s subp)) of
    Left {} -> assertFailure "Subterm path is not valid"
    Right n -> p n

eqStack :: StackId -> Term Natural -> Check ()
eqStack st = eqSubStack st []

unfoldTerm :: Term Natural -> NonEmpty (Term Natural)
unfoldTerm t = case t of
  TermAtom {} -> t :| []
  TermCell Cell {..} -> _cellLeft NonEmpty.<| unfoldTerm _cellRight

checkStackSize :: StackId -> Natural -> Check ()
checkStackSize st stSize = subStackPred st ([] :: Path) $ \s -> do
  let sl = NonEmpty.init (unfoldTerm s)
      lenSl = fromIntegral (length sl)
  unless (stSize == lenSl) (err lenSl)
  where
    err :: Natural -> Check ()
    err n = do
      let msg :: Text =
            "Expected "
              <> show st
              <> "\nto have size: "
              <> show stSize
              <> "\nbut has size: "
              <> show n
      assertFailure (unpack msg)

tests :: [Test]
tests =
  [ Test "push" (eqStack ValueStack [nock| [1 5 nil] |]) $ do
      pushNat 5
      pushNat 1,
    Test "pop" (eqStack ValueStack [nock| [1 nil] |]) $ do
      pushNat 1
      pushNat 33
      pop,
    Test "increment" (eqStack ValueStack [nock| [3 nil] |]) $ do
      pushNat 1
      increment
      increment,
    Test "dec" (eqStack ValueStack [nock| [5 nil] |]) $ do
      pushNat 6
      dec,
    Test "branch true" (eqStack ValueStack [nock| [5 nil] |]) $ do
      push (nockBoolLiteral True)
      branch (pushNat 5) (pushNat 666),
    Test "branch false" (eqStack ValueStack [nock| [666 nil] |]) $ do
      push (nockBoolLiteral False)
      branch (pushNat 5) (pushNat 666),
    Test "sub" (eqStack ValueStack [nock| [5 nil] |]) $ do
      pushNat 3
      pushNat 8
      callStdlib StdlibSub,
    Test "mul" (eqStack ValueStack [nock| [24 nil] |]) $ do
      pushNat 8
      pushNat 3
      callStdlib StdlibMul,
    Test "div" (eqStack ValueStack [nock| [3 nil] |]) $ do
      pushNat 5
      pushNat 15
      callStdlib StdlibDiv,
    Test "mod" (eqStack ValueStack [nock| [5 nil] |]) $ do
      pushNat 10
      pushNat 15
      callStdlib StdlibMod,
    Test "add" (eqStack ValueStack [nock| [5 nil] |]) $ do
      pushNat 2
      pushNat 3
      add,
    Test "pow2" (eqStack ValueStack [nock| [1 2 8 32 nil] |]) $ do
      pushNat 5
      pow2
      pushNat 3
      pow2
      pushNat 1
      pow2
      pushNat 0
      pow2,
    Test "append rights" (eqStack ValueStack [nock| [95 3 nil] |]) $ do
      push (OpQuote # toNock ([] :: Path))
      pushNat 1
      appendRights
      push (OpQuote # toNock [L])
      pushNat 5
      appendRights,
    Test "le less" (eqStack ValueStack [nock| [1 nil] |]) $ do
      pushNat 2
      pushNat 3
      callStdlib StdlibLe,
    Test "lt true" (eqStack ValueStack [nock| [0 nil] |]) $ do
      pushNat 4
      pushNat 3
      callStdlib StdlibLt,
    Test "lt eq" (eqStack ValueStack [nock| [1 nil] |]) $ do
      pushNat 3
      pushNat 3
      callStdlib StdlibLt,
    Test "le eq" (eqStack ValueStack [nock| [0 nil] |]) $ do
      pushNat 3
      pushNat 3
      callStdlib StdlibLe,
    Test "primitive eq true" (eqStack ValueStack [nock| [0 nil] |]) $ do
      pushNat 4
      pushNat 4
      testEq,
    Test "primitive eq false" (eqStack ValueStack [nock| [1 nil] |]) $ do
      pushNat 4
      pushNat 1
      testEq,
    Test
      "save"
      ( do
          eqStack ValueStack [nock| [67 2 nil] |]
          eqStack TempStack [nock| [77 nil] |]
      )
      $ do
        pushNat 2
        pushNat 3
        save False (pushNat 77)
        save True (pushNat 67),
    Test "primitive increment" (eqStack ValueStack [nock| [5 nil] |]) $ do
      pushNat 3
      increment
      increment,
    Test "call increment" (eqStack ValueStack [nock| [5 nil] |]) $ do
      pushNat 2
      callEnum FunIncrement 1
      callEnum FunIncrement 1
      callEnum FunIncrement 1,
    Test "call increment indirectly" (eqStack ValueStack [nock| [5 nil] |]) $ do
      pushNat 2
      callEnum FunIncrement 1
      callEnum FunCallInc 1
      callEnum FunIncrement 1,
    Test
      "push temp"
      ( do
          eqStack ValueStack [nock| [5 6 nil] |]
          eqStack TempStack [nock| [6 5 nil] |]
      )
      $ do
        pushNatOnto TempStack 5
        pushNatOnto TempStack 6
        pushTempRef 2 1
        pushTempRef 2 0,
    Test "push cell" (eqStack ValueStack [nock| [[1 2] nil] |]) $ do
      push (OpQuote # (1 :: Natural) # (2 :: Natural)),
    Test "push unit" (eqStack ValueStack [nock| [[0 nil nil] nil] |]) $ do
      push constUnit,
    Test "alloc nullary constructor" (eqStack ValueStack [nock| [[0 nil nil] nil] |]) $ do
      allocConstr (constructorTag ConstructorFalse),
    Test "alloc unary constructor" (eqStack ValueStack [nock| [[2 [[55 66] nil] nil] nil]|]) $ do
      push (OpQuote # (55 :: Natural) # (66 :: Natural))
      allocConstr (constructorTag ConstructorWrapper),
    Test "alloc binary constructor" (eqStack ValueStack [nock| [[3 [9 7 nil] nil] nil] |]) $ do
      pushNat 7
      pushNat 9
      allocConstr (constructorTag ConstructorPair),
    Test
      "alloc closure"
      ( do
          eqSubStack ValueStack (indexStack 0 ++ closurePath ClosureTotalArgsNum) [nock| 5 |]
          eqSubStack ValueStack (indexStack 0 ++ closurePath ClosureArgsNum) [nock| 3 |]
          eqSubStack ValueStack (indexStack 0 ++ closurePath ClosureArgs) [nock| [10 9 8 nil] |]
          eqSubStack ValueStack (indexStack 1) [nock| 7 |]
      )
      $ do
        pushNat 7
        pushNat 8
        pushNat 9
        pushNat 10
        allocClosure (sym FunConst5) 3,
    Test
      "alloc closure no args from value stack"
      ( do
          eqSubStack ValueStack (indexStack 0 ++ closurePath ClosureTotalArgsNum) [nock| 3 |]
          eqSubStack ValueStack (indexStack 0 ++ closurePath ClosureArgsNum) [nock| 0 |]
          eqSubStack ValueStack (indexStack 0 ++ closurePath ClosureArgs) [nock| nil |]
          checkStackSize ValueStack 1
      )
      $ allocClosure (sym FunAdd3) 0,
    Test
      "extend closure"
      ( do
          eqSubStack ValueStack (indexStack 0 ++ closurePath ClosureTotalArgsNum) [nock| 5 |]
          eqSubStack ValueStack (indexStack 0 ++ closurePath ClosureArgsNum) [nock| 3 |]
          eqSubStack ValueStack (indexStack 0 ++ closurePath ClosureArgs) [nock| [10 9 8 nil] |]
          eqSubStack ValueStack (indexStack 1) [nock| 7 |]
      )
      $ do
        pushNat 7
        pushNat 8
        pushNat 9
        pushNat 10
        allocClosure (sym FunConst5) 1
        extendClosure 2,
    Test "alloc, extend and call closure" (eqStack ValueStack [nock| [6 nil] |]) $
      do
        pushNat 1
        pushNat 2
        pushNat 3
        allocClosure (sym FunAdd3) 1
        extendClosure 1
        callHelper False Nothing 1,
    Test "call closure" (eqStack ValueStack [nock| [110 nil] |]) $
      do
        pushNat 100
        pushNat 110
        allocClosure (sym FunConst) 1
        callHelper False Nothing 1,
    Test
      "compute argsNum of a closure"
      (eqStack ValueStack [nock| [2 7 nil] |])
      $ do
        pushNat 7
        pushNat 8
        pushNat 9
        pushNat 10
        allocClosure (sym FunConst5) 3
        closureArgsNum,
    Test
      "save not tail"
      ( do
          eqStack ValueStack [nock| [17 nil] |]
          eqStack TempStack [nock| nil |]
      )
      $ do
        pushNat 10
        save False $ do
          pushNatOnto TempStack 7
          addOn TempStack
          moveTopFromTo TempStack ValueStack
          pushNatOnto TempStack 9,
    Test
      "save tail"
      ( do
          eqStack ValueStack [nock| [17 nil] |]
          eqStack TempStack [nock| [9 nil] |]
      )
      $ do
        pushNat 10
        save True $ do
          pushNatOnto TempStack 7
          addOn TempStack
          moveTopFromTo TempStack ValueStack
          pushNatOnto TempStack 9,
    Test
      "cmdCase: single branch"
      (eqStack ValueStack [nock| [777 [2 [123 nil] nil] nil] |])
      $ do
        pushNat 123
        allocConstr (constructorTag ConstructorWrapper)
        caseCmd
          Nothing
          [ (constructorTag ConstructorWrapper, pushNat 777)
          ],
    Test
      "cmdCase: default branch"
      (eqStack ValueStack [nock| [5 nil] |])
      $ do
        pushNat 123
        allocConstr (constructorTag ConstructorWrapper)
        caseCmd
          (Just (pop >> pushNat 5))
          [ (constructorTag ConstructorFalse, pushNat 777)
          ],
    Test
      "cmdCase: second branch"
      (eqStack ValueStack [nock| [5 nil] |])
      $ do
        pushNat 123
        allocConstr (constructorTag ConstructorWrapper)
        caseCmd
          (Just (pushNat 0))
          [ (constructorTag ConstructorFalse, pushNat 0),
            (constructorTag ConstructorWrapper, pop >> pushNat 5)
          ],
    Test
      "cmdCase: case on builtin true"
      (eqStack ValueStack [nock| [5 nil] |])
      $ do
        allocConstr (Asm.BuiltinTag Asm.TagTrue)
        caseCmd
          (Just (pushNat 0))
          [ (Asm.BuiltinTag Asm.TagTrue, pop >> pushNat 5),
            (Asm.BuiltinTag Asm.TagFalse, pushNat 0)
          ],
    Test
      "cmdCase: case on builtin false"
      (eqStack ValueStack [nock| [5 nil] |])
      $ do
        allocConstr (Asm.BuiltinTag Asm.TagFalse)
        caseCmd
          (Just (pushNat 0))
          [ (Asm.BuiltinTag Asm.TagTrue, pushNat 0),
            (Asm.BuiltinTag Asm.TagFalse, pop >> pushNat 5)
          ],
    Test
      "push constructor field"
      (eqStack TempStack [nock| [30 nil] |])
      $ do
        pushNat 10
        pushNat 20
        allocConstr (constructorTag ConstructorPair)
        pushConstructorFieldOnto TempStack Asm.StackRef 0
        pushConstructorFieldOnto TempStack Asm.StackRef 1
        addOn TempStack,
    Test
      "trace"
      ( do
          eqStack ValueStack [nock| [10 nil] |]
          eqTraces [[nock| 10 |]]
      )
      $ do
        pushNat 10
        traceTerm (OpAddress # topOfStack ValueStack)
  ]
