{-# LANGUAGE QuasiQuotes #-}

module Nockma.Compile.Positive where

import Base
import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Pretty
import Juvix.Compiler.Nockma.Translation.FromAsm
import Juvix.Compiler.Nockma.Translation.FromSource.QQ

data Test = Test
  { _testName :: Text,
    _testExpected :: Term Natural,
    _testProgram :: Sem '[Compiler, Embed IO] ()
  }

makeLenses ''Test

debugProgPrint :: Sem '[Compiler, Embed IO] () -> IO ()
debugProgPrint = debugProg >=> putStrLn . ppTrace

debugProg :: Sem '[Compiler, Embed IO] () -> IO (Term Natural)
debugProg m = runM $ runCompiledNock exampleFunctions takeValueStack
  where
    takeValueStack :: Sem '[Compiler, Embed IO] ()
    takeValueStack = do
      m
      verbatim (OpAddress # stackPath ValueStack)

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
      unless (n == _testExpected) (err n)
      where
        err :: Term Natural -> IO ()
        err n = do
          let msg =
                "Expected:\n"
                  <> ppTrace _testExpected
                  <> "But got:\n"
                  <> ppTrace n
          assertFailure (unpack msg)

tests :: [Test]
tests =
  [ Test "push" [nock| [1 5 0] |] $ do
      pushNat 5
      pushNat 1,
    Test "pop" [nock| [1 0] |] $ do
      pushNat 1
      pushNat 33
      pop,
    Test "increment" [nock| [3 0] |] $ do
      pushNat 1
      increment
      increment,
    Test "dec" [nock| [5 0] |] $ do
      pushNat 6
      dec,
    Test "branch true" [nock| [5 0] |] $ do
      push nockTrueLiteral
      branch (pushNat 5) (pushNat 666),
    Test "branch false" [nock| [666 0] |] $ do
      push nockFalseLiteral
      branch (pushNat 5) (pushNat 666),
    Test "add" [nock| [5 0] |] $ do
      pushNat 2
      pushNat 3
      add,
    Test "call increment" [nock| [5 0] |] $ do
      pushNat 2
      call "increment" 1
      call "increment" 1
      call "increment" 1,
    Test "call increment indirectly" [nock| [5 0] |] $ do
      pushNat 2
      call "increment" 1
      call "callInc" 1
      call "increment" 1
  ]
