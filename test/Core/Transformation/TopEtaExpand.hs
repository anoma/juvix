module Core.Transformation.TopEtaExpand (allTests) where

import Base
import Core.Eval.Positive qualified as Eval
import Core.Transformation.Base
import Juvix.Compiler.Core.Transformation

allTests :: TestTree
allTests = testGroup "Top eta expand" (mapMaybe liftTest Eval.tests)

pipe :: [TransformationId]
pipe = [TopEtaExpand]

liftTest :: Eval.PosTest -> Maybe TestTree
liftTest _testEval@Eval.PosTest {..}
  | _name `elem` excluded = Nothing
  | otherwise =
      Just $
        fromTest
          Test
            { _testTransformations = pipe,
              _testAssertion = const (return ()),
              _testEval
            }

excluded :: [String]
excluded =
  []
