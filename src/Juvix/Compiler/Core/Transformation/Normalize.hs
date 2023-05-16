module Juvix.Compiler.Core.Transformation.Normalize where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Normalizer qualified as Normalizer
import Juvix.Compiler.Core.Transformation.Base

normalize :: InfoTable -> InfoTable
normalize tab =
  pruneInfoTable $
    set identContext (HashMap.singleton sym node) $
      set infoIdentifiers (HashMap.singleton sym ii) tab
  where
    sym = fromJust $ tab ^. infoMain
    node = Normalizer.normalize tab (lookupIdentifierNode tab sym)
    ii = lookupIdentifierInfo tab sym
