module Juvix.Compiler.Core.Transformation.Optimize.Inlining where

import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Transformation.Base

convertNode :: InfoTable -> Node -> Node
convertNode tab = dmap go
  where
    go :: Node -> Node
    go node = case node of
      NApp {} ->
        let (h, args) = unfoldApps node
         in case h of
              NIdt Ident {..} ->
                case pi of
                  Just InlineFullyApplied
                    | length args >= argsNum ->
                        mkApps def args
                  Just (InlinePartiallyApplied k)
                    | length args >= k ->
                        mkApps def args
                  _ ->
                    node
                where
                  ii = lookupIdentifierInfo tab _identSymbol
                  pi = ii ^. identifierPragmas . pragmasInline
                  argsNum = ii ^. identifierArgsNum
                  def = lookupIdentifierNode tab _identSymbol
              _ ->
                node
      _ ->
        node

inlining :: InfoTable -> InfoTable
inlining tab = mapT (const (convertNode tab)) tab
