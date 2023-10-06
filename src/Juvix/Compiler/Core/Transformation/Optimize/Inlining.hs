module Juvix.Compiler.Core.Transformation.Optimize.Inlining where

import Data.HashSet qualified as HashSet
import Juvix.Compiler.Core.Data.BinderList qualified as BL
import Juvix.Compiler.Core.Data.IdentDependencyInfo
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Options
import Juvix.Compiler.Core.Transformation.Base

isInlineableLambda :: Int -> InfoTable -> BinderList Binder -> Node -> Bool
isInlineableLambda inlineDepth tab bl node = case node of
  NLam {} ->
    let (lams, body) = unfoldLambdas node
        binders = map (^. lambdaLhsBinder) lams
     in checkDepth tab (BL.prependRev binders bl) inlineDepth body
  _ ->
    False

convertNode :: Int -> HashSet Symbol -> InfoTable -> Node -> Node
convertNode inlineDepth recSyms tab = dmapL go
  where
    go :: BinderList Binder -> Node -> Node
    go bl node = case node of
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
                        && isInlineableLambda inlineDepth tab bl def
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
      NIdt Ident {..} ->
        case pi of
          Just InlineFullyApplied | argsNum == 0 -> def
          Just (InlinePartiallyApplied 0) -> def
          _ -> node
        where
          ii = lookupIdentifierInfo tab _identSymbol
          pi = ii ^. identifierPragmas . pragmasInline
          argsNum = ii ^. identifierArgsNum
          def = lookupIdentifierNode tab _identSymbol
      -- inline zero-argument definitions automatically if inlining would result
      -- in case reduction
      NCase cs@Case {..} -> case _caseValue of
        NIdt Ident {..}
          | isNothing pi
              && not (HashSet.member _identSymbol recSyms)
              && isConstructorApp def
              && checkDepth tab bl inlineDepth def ->
              NCase cs {_caseValue = def}
          where
            ii = lookupIdentifierInfo tab _identSymbol
            pi = ii ^. identifierPragmas . pragmasInline
            def = lookupIdentifierNode tab _identSymbol
        _ ->
          node
      _ ->
        node

inlining' :: Int -> HashSet Symbol -> InfoTable -> InfoTable
inlining' inliningDepth recSyms tab = mapT (const (convertNode inliningDepth recSyms tab)) tab

inlining :: (Member (Reader CoreOptions) r) => InfoTable -> Sem r InfoTable
inlining tab = do
  d <- asks (^. optInliningDepth)
  return $ inlining' d (recursiveIdents tab) tab
