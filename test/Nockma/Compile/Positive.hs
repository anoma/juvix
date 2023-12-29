{-# LANGUAGE QuasiQuotes #-}

module Nockma.Compile.Positive where

import Base
import Juvix.Compiler.Nockma.Evaluator
import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Pretty
import Juvix.Compiler.Nockma.Translation.FromAsm
import Juvix.Compiler.Nockma.Translation.FromSource.QQ

type Check = Sem '[Reader (Term Natural), Embed IO]

data Test = Test
  { _testName :: Text,
    _testCheck :: Check (),
    _testProgram :: Sem '[Compiler, Embed IO] ()
  }

makeLenses ''Test

debugProg :: Sem '[Compiler, Embed IO] () -> IO (Term Natural)
debugProg = runM . runCompiledNock exampleFunctions

exampleFunctions :: [CompilerFunction]
exampleFunctions =
  [ CompilerFunction "increment" compileFunIncrement,
    CompilerFunction "const" compileFunConst,
    CompilerFunction "callInc" compileCallInc
  ]

allTests :: TestTree
allTests = testGroup "Nockma compile unit positive" (map mk tests)
  where
    mk :: Test -> TestTree
    mk Test {..} = testCase (unpack _testName) $ do
      n <- debugProg _testProgram
      runM (runReader n _testCheck)

getStack :: StackId -> Term Natural -> Term Natural
getStack st m = fromRight' (run (runError @NockEvalError (subTerm m (stackPath st))))

eqStack :: StackId -> Term Natural -> Check ()
eqStack st expected = do
  n <- getStack st <$> ask
  unless (n == expected) (err n)
  where
    err :: Term Natural -> Check ()
    err n = do
      let msg =
            "Expected: "
              <> show st
              <> "\n"
              <> ppTrace expected
              <> "But got:\n"
              <> ppTrace n
      assertFailure (unpack msg)

tests :: [Test]
tests =
  [ Test "push" (eqStack ValueStack [nock| [1 5 0] |]) $ do
      pushNat 5
      pushNat 1,
    Test "pop" (eqStack ValueStack [nock| [1 0] |]) $ do
      pushNat 1
      pushNat 33
      pop,
    Test "increment" (eqStack ValueStack [nock| [3 0] |]) $ do
      pushNat 1
      increment
      increment,
    Test "dec" (eqStack ValueStack [nock| [5 0] |]) $ do
      pushNat 6
      dec,
    Test "branch true" (eqStack ValueStack [nock| [5 0] |]) $ do
      push (nockBoolLiteral True)
      branch (pushNat 5) (pushNat 666),
    Test "branch false" (eqStack ValueStack [nock| [666 0] |]) $ do
      push (nockBoolLiteral False)
      branch (pushNat 5) (pushNat 666),
    Test "sub" (eqStack ValueStack [nock| [5 0] |]) $ do
      pushNat 3
      pushNat 8
      callStdlib StdlibSub,
    Test "mul" (eqStack ValueStack [nock| [24 0] |]) $ do
      pushNat 8
      pushNat 3
      callStdlib StdlibMul,
    Test "div" (eqStack ValueStack [nock| [3 0] |]) $ do
      pushNat 5
      pushNat 15
      callStdlib StdlibDiv,
    Test "mod" (eqStack ValueStack [nock| [5 0] |]) $ do
      pushNat 10
      pushNat 15
      callStdlib StdlibMod,
    Test "add" (eqStack ValueStack [nock| [5 0] |]) $ do
      pushNat 2
      pushNat 3
      add,
    Test "le less" (eqStack ValueStack [nock| [1 0] |]) $ do
      pushNat 2
      pushNat 3
      callStdlib StdlibLe,
    Test "lt true" (eqStack ValueStack [nock| [0 0] |]) $ do
      pushNat 4
      pushNat 3
      callStdlib StdlibLt,
    Test "lt eq" (eqStack ValueStack [nock| [1 0] |]) $ do
      pushNat 3
      pushNat 3
      callStdlib StdlibLt,
    Test "le eq" (eqStack ValueStack [nock| [0 0] |]) $ do
      pushNat 3
      pushNat 3
      callStdlib StdlibLe,
    Test "primitive eq true" (eqStack ValueStack [nock| [0 0] |]) $ do
      pushNat 4
      pushNat 4
      testEq,
    Test "primitive eq false" (eqStack ValueStack [nock| [1 0] |]) $ do
      pushNat 4
      pushNat 1
      testEq,
    Test "primitive increment" (eqStack ValueStack [nock| [5 0] |]) $ do
      pushNat 3
      increment
      increment,
    Test "call increment" (eqStack ValueStack [nock| [5 0] |]) $ do
      pushNat 2
      call "increment" 1
      call "increment" 1
      call "increment" 1,
    Test "call increment indirectly" (eqStack ValueStack [nock| [5 0] |]) $ do
      pushNat 2
      call "increment" 1
      call "callInc" 1
      call "increment" 1
  ]
