module Juvix.Compiler.Core.Transformation.FoldTypeSynonyms where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Transformation.Base

convertNode :: InfoTable -> Node -> Node
convertNode tab = rmap go
  where
    go :: ([BinderChange] -> Node -> Node) -> Node -> Node
    go recur = \case
      NIdt Ident {..}
        | isTypeConstr tab (ii ^. identifierType) ->
            go recur $ fromJust $ HashMap.lookup _identSymbol (tab ^. identContext)
        where
          ii = fromJust $ HashMap.lookup _identSymbol (tab ^. infoIdentifiers)
      NLet Let {..}
        | isTypeConstr tab (_letItem ^. letItemBinder . binderType) ->
            go (recur . (mkBCRemove (_letItem ^. letItemBinder) val' :)) _letBody
        where
          val' = go recur (_letItem ^. letItemValue)
      node ->
        recur [] node

foldTypeSynonyms :: InfoTable -> InfoTable
foldTypeSynonyms tab =
  filterOutTypeSynonyms $
    mapAllNodes (convertNode tab) tab
