module Nockma.Compile.Asm.Positive where

import Asm.Run.Base
import Asm.Run.Positive qualified as Asm
import Base
import Juvix.Compiler.Asm
import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Pretty qualified as Nockma
import Juvix.Compiler.Nockma.Translation.FromAsm
import Juvix.Compiler.Nockma.Translation.FromAsm qualified as Nockma

runNockmaAssertion :: Handle -> Symbol -> InfoTable -> IO ()
runNockmaAssertion hout sym tab = do
  tab' <- runM $ runErrorIO' @AsmError (computeApply tab)
  let (nockSubject, nockMain) = Nockma.fromAsm sym tab'
  res <- runM $ runOutputSem @(Term Natural) (embed . hPutStrLn hout . Nockma.ppPrint) (evalCompiledNock' nockSubject nockMain)
  let ret = getReturn res
  hPutStrLn hout (Nockma.ppPrint ret)
  where
    getReturn :: Term Natural -> Term Natural
    getReturn res =
      let valStack = getStack ValueStack res
       in case valStack of
            TermCell c -> c ^. cellLeft
            TermAtom {} -> error "should be a cell"

testDescr :: Asm.PosTest -> TestDescr
testDescr Asm.PosTest {..} =
  let tRoot = Asm.root <//> _relDir
      file' = tRoot <//> _file
      expected' = tRoot <//> _expectedFile
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ asmRunAssertionParam runNockmaAssertion file' expected' return (const (return ()))
        }

testsToRun :: [String]
testsToRun =
  [ "Test001: Arithmetic opcodes",
    "Test002: Direct call",
    "Test003: Indirect call",
    "Test004: Tail calls"
  ]

allTests :: TestTree
allTests =
  testGroup
    "Nockma Asm eval positive tests"
    (map (mkTest . testDescr) (Asm.filterTests testsToRun Asm.tests))
