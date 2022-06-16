module MiniJuvix.Utils.Paths where

import Data.FileEmbed qualified as FE
import Language.Haskell.TH.Syntax
import MiniJuvix.Prelude

assetsDir :: Q Exp
assetsDir = FE.makeRelativeToProject "assets" >>= FE.embedDir
