module MiniJuvix.Utils.Paths where

import Language.Haskell.TH.Syntax as TH
import MiniJuvix.Prelude
import TH.RelativePaths

assetsDir :: Q Exp
assetsDir = pathRelativeToCabalPackage "assets" >>= TH.lift
