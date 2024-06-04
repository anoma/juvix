module Tree.Transformation.CheckNoAnoma where

import Base
import Juvix.Compiler.Tree.Error
import Juvix.Compiler.Tree.Transformation as Tree
import Juvix.Compiler.Tree.Translation.FromSource
import Juvix.Data.PPOutput
import Tree.Eval.Negative qualified as Eval

data CheckNoAnomaTest = CheckNoAnomaTest
  { _testEval :: Eval.NegTest
  }

fromTest :: CheckNoAnomaTest -> TestTree
fromTest = mkTest . toTestDescr

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/Tree/negative/")

treeEvalTransformationErrorAssertion ::
  Path Abs File ->
  [TransformationId] ->
  (JuvixError -> IO ()) ->
  (String -> IO ()) ->
  Assertion
treeEvalTransformationErrorAssertion mainFile trans checkError step = do
  step "Parse"
  s <- readFile mainFile
  case runParser mainFile s of
    Left err -> assertFailure (prettyString err)
    Right tab0 -> do
      step "Validate"
      case run $ runError @JuvixError $ applyTransformations [Validate] tab0 of
        Left err -> assertFailure (prettyString (fromJuvixError @GenericError err))
        Right tab1 -> do
          unless (null trans) $
            step "Transform"
          case run $ runError @JuvixError $ applyTransformations trans tab1 of
            Left e -> checkError e
            Right {} -> assertFailure "Expected error"

toTestDescr :: CheckNoAnomaTest -> TestDescr
toTestDescr CheckNoAnomaTest {..} =
  let Eval.NegTest {..} = _testEval
      tRoot = root <//> _relDir
      file' = tRoot <//> _file
      checkError :: JuvixError -> IO ()
      checkError e =
        unless
          (isJust (fromJuvixError @TreeError e))
          (assertFailure (unpack ("Expected TreeError. got: " <> renderTextDefault e)))
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ treeEvalTransformationErrorAssertion file' [CheckNoAnoma] checkError
        }

allTests :: TestTree
allTests = testGroup "CheckNoAnoma" (map (fromTest . CheckNoAnomaTest) tests)

tests :: [Eval.NegTest]
tests =
  [ Eval.NegTest
      "anomaGet"
      $(mkRelDir ".")
      $(mkRelFile "test009.jvt"),
    Eval.NegTest
      "anomaDecode"
      $(mkRelDir ".")
      $(mkRelFile "test010.jvt"),
    Eval.NegTest
      "anomaEncode"
      $(mkRelDir ".")
      $(mkRelFile "test011.jvt"),
    Eval.NegTest
      "anomaVerifyDetached"
      $(mkRelDir ".")
      $(mkRelFile "test012.jvt"),
    Eval.NegTest
      "anomaSign"
      $(mkRelDir ".")
      $(mkRelFile "test013.jvt"),
    Eval.NegTest
      "anomaVerify"
      $(mkRelDir ".")
      $(mkRelFile "test014.jvt"),
    Eval.NegTest
      "anomaSignDetached"
      $(mkRelDir ".")
      $(mkRelFile "test015.jvt")
  ]
