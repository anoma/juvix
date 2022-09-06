module Juvix.Compiler.Core.Translation.Stripped.FromCore where

import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Data.Stripped.InfoTable qualified as Stripped
import Juvix.Compiler.Core.Extra.Stripped.Base qualified as Stripped
import Juvix.Compiler.Core.Language
import Juvix.Compiler.Core.Language.Stripped qualified as Stripped

fromMain :: InfoTable -> Stripped.InfoTable
fromMain = error "not yet implemented"

translateNode :: Node -> Stripped.Node
translateNode _ = Stripped.mkVar' 0
