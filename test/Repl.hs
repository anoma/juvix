module Repl where

import Base
import Repl.Positive qualified as P

allTests :: TestTree
allTests = testGroup "Juvix REPL tests" [P.allTests]
