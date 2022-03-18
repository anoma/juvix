module MiniJuvix.Utils.Paths where

import MiniJuvix.Prelude
import TH.RelativePaths
import Language.Haskell.TH.Syntax as TH

assetsDir :: Q Exp
assetsDir = pathRelativeToCabalPackage "assets" >>= TH.lift
