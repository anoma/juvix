module Juvix.Compiler.Core.Transformation.Normalize where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Normalizer qualified as Normalizer
import Juvix.Compiler.Core.Options
import Juvix.Compiler.Core.Transformation.Base

normalize :: (Member (Reader CoreOptions) r) => Module -> Sem r Module
normalize md = do
  opts <- ask
  let node = Normalizer.normalize (opts ^. optFieldSize) md (lookupIdentifierNode md sym)
  return $
    pruneInfoTable $
      set (moduleInfoTable . identContext) (HashMap.singleton sym node) $
        set (moduleInfoTable . infoIdentifiers) (HashMap.singleton sym ii) md
  where
    sym = fromJust $ getInfoMain md
    ii = lookupIdentifierInfo md sym
