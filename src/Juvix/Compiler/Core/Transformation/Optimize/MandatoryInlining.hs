module Juvix.Compiler.Core.Transformation.Optimize.MandatoryInlining where

import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Transformation.Base

convertNode :: Module -> Node -> Node
convertNode md = dmap go
  where
    go :: Node -> Node
    go node = case node of
      NIdt Ident {..}
        | Just InlineAlways <- lookupIdentifierInfo md _identSymbol ^. identifierPragmas . pragmasInline ->
            lookupIdentifierNode md _identSymbol
      NCase cs@Case {..} -> case _caseValue of
        NIdt Ident {..}
          | Just InlineCase <- lookupIdentifierInfo md _identSymbol ^. identifierPragmas . pragmasInline ->
              NCase cs {_caseValue = lookupIdentifierNode md _identSymbol}
        _ ->
          node
      _ ->
        node

mandatoryInlining :: Module -> Module
mandatoryInlining md = mapAllNodes (convertNode md) md
