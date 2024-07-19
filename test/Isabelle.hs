module Isabelle
  ( allTests,
  )
where

import Base
import Isabelle.Positive qualified as P

allTests :: TestTree
allTests = testGroup "Isabelle tests" [P.allTests]
