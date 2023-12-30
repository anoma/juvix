module Juvix.Compiler.Core.Transformation.FoldTypeSynonyms where

import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Transformation.Base

convertNode :: Module -> Node -> Node
convertNode md = rmap go
  where
    go :: ([BinderChange] -> Node -> Node) -> Node -> Node
    go recur = \case
      NIdt Ident {..}
        | isTypeConstr md (ii ^. identifierType) ->
            go recur $ lookupIdentifierNode md _identSymbol
        where
          ii = lookupIdentifierInfo md _identSymbol
      NLet Let {..}
        | isTypeConstr md (_letItem ^. letItemBinder . binderType) ->
            go (recur . (mkBCRemove (_letItem ^. letItemBinder) val' :)) _letBody
        where
          val' = go recur (_letItem ^. letItemValue)
      node ->
        recur [] node

foldTypeSynonyms :: Module -> Module
foldTypeSynonyms md =
  filterOutTypeSynonyms $
    mapAllNodes (convertNode md) md
