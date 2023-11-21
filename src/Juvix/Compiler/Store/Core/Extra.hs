module Juvix.Compiler.Store.Core.Extra where

import Juvix.Compiler.Core.Data.InfoTable qualified as Core
import Juvix.Compiler.Core.Language qualified as Core
import Juvix.Compiler.Store.Core.Data.InfoTable
import Juvix.Compiler.Store.Core.Language
import Juvix.Prelude

toCore :: InfoTable -> Core.InfoTable
toCore tab =
  Core.InfoTable
    { _identContext = fmap (goNode . (^. identifierNode)) (tab ^. infoIdentifiers),
      _identMap = undefined
    }
  where
    goNode :: Node -> Core.Node
    goNode = \case {}
