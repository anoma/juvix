module Juvix.Compiler.Core.Transformation.Normalize where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Normalizer qualified as Normalizer
import Juvix.Compiler.Core.Transformation.Base

normalize :: Module -> Module
normalize md =
  pruneInfoTable $
    set (moduleInfoTable . identContext) (HashMap.singleton sym node) $
      set (moduleInfoTable . infoIdentifiers) (HashMap.singleton sym ii) md
  where
    sym = fromJust $ getInfoMain md
    node = Normalizer.normalize md (lookupIdentifierNode md sym)
    ii = lookupIdentifierInfo md sym
