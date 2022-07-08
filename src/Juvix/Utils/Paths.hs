module Juvix.Utils.Paths where

import Data.FileEmbed qualified as FE
import Juvix.Prelude
import Language.Haskell.TH.Syntax

assetsDir :: Q Exp
assetsDir = FE.makeRelativeToProject "assets" >>= FE.embedDir
