module Core.VampIR.Positive where

import Base
import Core.Normalize.Positive (PosTest (..))
import Core.Normalize.Positive qualified as Normalize
import Core.VampIR.Base
import Juvix.Compiler.Core.Data.TransformationId

fromTest :: PosTest -> TestTree
fromTest = mkTest . toTestDescr

toTestDescr :: PosTest -> TestDescr
toTestDescr = Normalize.toTestDescr' (const (coreVampIRAssertion toVampIRTransformations))

allTests :: TestTree
allTests =
  testGroup
    "JuvixCore VampIR positive tests"
    (map (mkTest . toTestDescr) Normalize.tests)
