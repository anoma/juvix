module Juvix.Compiler.Core.Transformation.Optimize.Inlining where

import Data.HashSet qualified as HashSet
import Juvix.Compiler.Core.Data.IdentDependencyInfo
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Options
import Juvix.Compiler.Core.Transformation.Base

isInlineableLambda :: Int -> Node -> Bool
isInlineableLambda inlineDepth node = case node of
  NLam {} ->
    checkDepth inlineDepth (snd (unfoldLambdas node))
  _ ->
    False

convertNode :: Int -> HashSet Symbol -> InfoTable -> Node -> Node
convertNode inlineDepth recSyms tab = dmap go
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
                  Just InlineNever ->
                    node
                  _
                    | not (HashSet.member _identSymbol recSyms)
                        && isInlineableLambda inlineDepth def
                        && length args >= argsNum ->
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

inlining' :: Int -> InfoTable -> InfoTable
inlining' inliningDepth tab = mapT (const (convertNode inliningDepth recursiveIdents tab)) tab
  where
    recursiveIdents = nodesOnCycles (createIdentDependencyInfo tab)

inlining :: Member (Reader CoreOptions) r => InfoTable -> Sem r InfoTable
inlining tab = do
  d <- asks (^. optInliningDepth)
  return $ inlining' d tab
