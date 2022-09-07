module Core.Transformation.Lifting (allTests) where

import Base
import Core.Transformation.Base
import Juvix.Compiler.Core.Transformation

allTests :: TestTree
allTests = testGroup "Lambda lifting" tests

pipe :: [TransformationId]
pipe = [LambdaLifting]

dir :: FilePath
dir = "lambda-lifting"

liftTest :: String -> FilePath -> FilePath -> TestTree
liftTest _testName _testCoreFile _testExpectedFile =
  fromTest
    Test
      { _testTransformations = pipe,
        _testCoreFile = dir </> _testCoreFile,
        _testName,
        _testAssertion = assertExpectedOutput expectedFile
      }
      where
      expectedFile = dir </> _testExpectedFile

tests :: [TestTree]
tests =
  [ liftTest
      ("Lambda lifting without let rec " <> i)
      ("test" <> i <> ".jvc")
      ("test" <> i <> ".out")
    | i <- map show [1 :: Int .. 3]
  ]
