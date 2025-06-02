module Core.Print.Base where

import Base
import Core.Eval.Base
import Juvix.Compiler.Core.Data.Module (computeCombinedInfoTable, moduleFromInfoTable)
import Juvix.Compiler.Core.Pretty
import Juvix.Compiler.Core.Transformation.DisambiguateNames (disambiguateNames)
import Juvix.Compiler.Core.Translation.FromSource

corePrintAssertion ::
  Path Abs File ->
  Path Abs File ->
  (String -> IO ()) ->
  Assertion
corePrintAssertion mainFile expectedFile step = do
  step "Parse"
  r <- parseFile mainFile
  case r of
    Left err -> assertFailure (prettyString (fromJuvixError @GenericError err))
    Right (_, Nothing) -> do
      step "Empty program: compare expected and actual program output"
      expected <- readFile expectedFile
      assertEqDiffText ("Check: EVAL output = " <> toFilePath expectedFile) "" expected
    Right (tabIni, Just node) -> do
      let m = disambiguateNames (moduleFromInfoTable $ setupMainFunction defaultModuleId tabIni node)
          tab = computeCombinedInfoTable m
      step "Print and parse back"
      let r' = runParserMain mainFile defaultModuleId mempty (ppPrint tab)
      case r' of
        Left err -> assertFailure (prettyString (fromJuvixError @GenericError err))
        Right tab' -> coreEvalAssertion' EvalModePlain tab' mainFile expectedFile step

coreParseErrorAssertion :: Path Abs File -> (String -> IO ()) -> Assertion
coreParseErrorAssertion mainFile step = do
  step "Parse"
  r <- parseFile mainFile
  case r of
    Left _ -> return ()
    Right _ -> assertFailure "no error"
