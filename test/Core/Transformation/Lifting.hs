module Core.Transformation.Lifting (allTests) where

import Base
import Core.Eval.Positive qualified as Eval
import Core.Transformation.Base
import Juvix.Compiler.Core.Transformation

allTests :: TestTree
allTests = testGroup "Lambda lifting" (mapMaybe liftTest Eval.tests)

pipe :: [TransformationId]
pipe = [LambdaLifting]

liftTest :: Eval.PosTest -> Maybe TestTree
liftTest _testEval@Eval.PosTest {..}
  | _name `elem` excluded = Nothing
  | otherwise =
      Just $
        fromTest
          Test
            { _testTransformations = pipe,
              _testAssertion = \i -> unless (isLifted i) (error ""),
              _testEval
            }

excluded :: [String]
excluded =
  [ "Functions returning functions with variable capture",
    "Higher-order recursive functions",
    "Ackermann function (higher-order definition)",
    "LetRec"
  ]
