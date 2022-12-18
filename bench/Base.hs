module Base where

import Juvix.Extra.Paths
import Juvix.Prelude.Path as Path

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/benchmark")
