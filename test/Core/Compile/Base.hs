module Core.Compile.Base where

import Asm.Compile.Base qualified as Asm
import Base
import Core.Eval.Base
import Core.Eval.Positive qualified as Eval
import Data.Text.IO qualified as TIO
import Juvix.Compiler.Asm.Translation.FromCore qualified as Asm
import Juvix.Compiler.Core.Pipeline
import Juvix.Compiler.Core.Translation.FromSource
import Juvix.Compiler.Core.Translation.Stripped.FromCore qualified as Stripped
import Juvix.Data.PPOutput

newtype Test = Test
  { _testEval :: Eval.PosTest
  }

fromTest :: Test -> TestTree
fromTest = mkTest . toTestDescr

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/Core/positive/")

toTestDescr :: Test -> TestDescr
toTestDescr Test {..} =
  let Eval.PosTest {..} = _testEval
      tRoot = root <//> _relDir
      file' = tRoot <//> _file
      expected' = tRoot <//> _expectedFile
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ coreCompileAssertion file' expected'
        }

coreCompileAssertion ::
  Path Abs File ->
  Path Abs File ->
  (String -> IO ()) ->
  Assertion
coreCompileAssertion mainFile expectedFile step = do
  step "Parse"
  r <- parseFile mainFile
  case r of
    Left err -> assertFailure (show (pretty err))
    Right (_, Nothing) -> do
      step "Empty program: compare expected and actual program output"
      expected <- TIO.readFile (toFilePath expectedFile)
      assertEqDiff ("Check: EVAL output = " <> toFilePath expectedFile) "" expected
    Right (tabIni, Just node) -> do
      step "Translate to JuvixAsm"
      let tab = Asm.fromCore $ Stripped.fromCore $ toStripped $ setupMainFunction tabIni node
      Asm.asmCompileAssertion' tab mainFile expectedFile step
