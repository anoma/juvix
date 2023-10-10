module Juvix.Compiler.Core.Transformation.Optimize.CaseValueInlining where

import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Transformation.Base

convertNode :: InfoTable -> Node -> Node
convertNode tab = dmap go
  where
    go :: Node -> Node
    go node = case node of
      NCase cs@Case {..} -> case _caseValue of
        NIdt Ident {..}
          | Just InlineCase <- lookupIdentifierInfo tab _identSymbol ^. identifierPragmas . pragmasInline ->
              NCase cs {_caseValue = lookupIdentifierNode tab _identSymbol}
        _ ->
          node
      _ ->
        node

caseValueInlining :: InfoTable -> InfoTable
caseValueInlining tab = mapAllNodes (convertNode tab) tab
