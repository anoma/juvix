module Core.Compile.Base where

import Asm.Compile.Base qualified as Asm
import Base
import Core.Eval.Base
import Core.Eval.Positive qualified as Eval
import GHC.Base (seq)
import Juvix.Compiler.Asm.Pretty qualified as Asm
import Juvix.Compiler.Asm.Translation.FromTree qualified as Asm
import Juvix.Compiler.Core.Data.Module
import Juvix.Compiler.Core.Data.TransformationId
import Juvix.Compiler.Core.Extra.Utils
import Juvix.Compiler.Core.Options
import Juvix.Compiler.Core.Pipeline
import Juvix.Compiler.Core.Translation.FromSource
import Juvix.Compiler.Core.Translation.Stripped.FromCore qualified as Stripped
import Juvix.Compiler.Tree.Translation.FromCore qualified as Tree
import Juvix.Data.Field
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
          _testAssertion = Steps $ coreCompileAssertion file' expected' ""
        }

coreCompileAssertion' ::
  Int ->
  InfoTable ->
  Path Abs File ->
  Path Abs File ->
  Text ->
  (String -> IO ()) ->
  Assertion
coreCompileAssertion' optLevel tab mainFile expectedFile stdinText step = do
  step "Translate to JuvixAsm"
  case run . runReader opts . runError $ toStored' (moduleFromInfoTable tab) >>= toStripped' CheckExec of
    Left err -> assertFailure (prettyString (fromJuvixError @GenericError err))
    Right m -> do
      let tab0 = computeCombinedInfoTable m
      assertBool "Check info table" (checkInfoTable tab0)
      let tab' = Asm.fromTree . Tree.fromCore $ Stripped.fromCore (maximum allowedFieldSizes) tab0
      length (fromText (Asm.ppPrint tab' tab') :: String) `seq`
        Asm.asmCompileAssertion' optLevel tab' mainFile expectedFile stdinText step
  where
    opts = defaultCoreOptions {_optOptimizationLevel = optLevel}

coreCompileAssertion ::
  Path Abs File ->
  Path Abs File ->
  Text ->
  (String -> IO ()) ->
  Assertion
coreCompileAssertion mainFile expectedFile stdinText step = do
  step "Parse"
  r <- parseFile mainFile
  case r of
    Left err -> assertFailure (prettyString err)
    Right (_, Nothing) -> do
      step "Empty program: compare expected and actual program output"
      expected <- readFile expectedFile
      assertEqDiffText ("Check: EVAL output = " <> toFilePath expectedFile) "" expected
    Right (tabIni, Just node) ->
      coreCompileAssertion' 3 (setupMainFunction defaultModuleId tabIni node) mainFile expectedFile stdinText step
