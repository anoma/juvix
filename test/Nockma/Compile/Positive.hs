{-# LANGUAGE QuasiQuotes #-}

module Nockma.Compile.Positive where

import Base hiding (Path)
import Juvix.Compiler.Asm.Language qualified as Asm
import Juvix.Compiler.Nockma.Evaluator
import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Pretty
import Juvix.Compiler.Nockma.Translation.FromAsm
import Juvix.Compiler.Nockma.Translation.FromSource.QQ

type Check = Sem '[Reader (Term Natural), Embed IO]

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
  deriving stock (Eq, Bounded, Enum)

sym :: (Enum a) => a -> Asm.Symbol
sym = Asm.defaultSymbol . fromIntegral . fromEnum

debugProg :: Sem '[Compiler] () -> Term Natural
debugProg mkMain = compileAndRunNock exampleConstructors exampleFunctions mainFun
  where
    mainFun =
      CompilerFunction
        { _compilerFunctionName = sym FunMain,
          _compilerFunctionArity = 0,
          _compilerFunction = raiseUnder mkMain
        }

isMain :: FunctionName -> Bool
isMain = (== FunMain)

functionArity' :: FunctionName -> Natural
functionArity' = \case
  FunMain -> 0
  FunIncrement -> 1
  FunConst -> 2
  FunCallInc -> 1
  FunConst5 -> 5

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

-- | NOTE that new constructors should be added to the end of the list or else
-- the tests will fail.
data ConstructorName
  = ConstructorFalse
  | ConstructorTrue
  | ConstructorWrapper
  | ConstructorPair
  deriving stock (Eq, Bounded, Enum)

constructorTag :: ConstructorName -> Asm.Tag
constructorTag n = Asm.UserTag defaultModuleId (fromIntegral (fromEnum n))

constructorArity :: ConstructorName -> Natural
constructorArity = \case
  ConstructorFalse -> 0
  ConstructorTrue -> 0
  ConstructorWrapper -> 1
  ConstructorPair -> 2

exampleConstructors :: ConstructorArities
exampleConstructors =
  hashMap
    [ (constructorTag n, constructorArity n)
      | n <- allElements
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
      let n = debugProg _testProgram
      runM (runReader n _testCheck)

eqSubStack :: StackId -> Path -> Term Natural -> Check ()
eqSubStack st subp expected = do
  s <- getStack st <$> ask
  case run (runError @NockEvalError (subTerm s subp)) of
    Left {} -> assertFailure "Subterm path is not valid"
    Right n -> unless (n == expected) (err n)
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

eqStack :: StackId -> Term Natural -> Check ()
eqStack st = eqSubStack st []

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
      "push constructor field"
      (eqStack TempStack [nock| [30 nil] |])
      $ do
        pushNat 10
        pushNat 20
        allocConstr (constructorTag ConstructorPair)
        pushConstructorFieldOnto TempStack Asm.StackRef 0
        pushConstructorFieldOnto TempStack Asm.StackRef 1
        addOn TempStack
  ]
