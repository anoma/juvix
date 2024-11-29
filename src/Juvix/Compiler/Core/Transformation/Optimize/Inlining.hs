module Juvix.Compiler.Core.Transformation.Optimize.Inlining where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Core.Data.BinderList qualified as BL
import Juvix.Compiler.Core.Data.IdentDependencyInfo
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Options
import Juvix.Compiler.Core.Transformation.Base

isInlineableLambda :: Int -> Module -> BinderList Binder -> Node -> Bool
isInlineableLambda inlineDepth md bl node = case node of
  NLam {} ->
    let (lams, body) = unfoldLambdas node
        binders = map (^. lambdaLhsBinder) lams
     in checkDepth md (BL.prependRev binders bl) inlineDepth body
  _ ->
    False

convertNode :: Int -> HashSet Symbol -> HashMap Symbol Int -> Module -> Node -> Node
convertNode inlineDepth nonRecSyms symOcc md = dmapL go
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
                  Just InlineAlways ->
                    mkApps def args
                  Just InlineNever ->
                    node
                  _
                    | HashSet.member _identSymbol nonRecSyms
                        && length args >= argsNum
                        && ( HashMap.lookup _identSymbol symOcc == Just 1
                               || isInlineableLambda inlineDepth md bl def
                           ) ->
                        mkApps def args
                  _ ->
                    node
                where
                  ii = lookupIdentifierInfo md _identSymbol
                  pi = ii ^. identifierPragmas . pragmasInline
                  argsNum = ii ^. identifierArgsNum
                  def = lookupIdentifierNode md _identSymbol
              _ ->
                node
      NIdt Ident {..} ->
        case pi of
          Just InlineFullyApplied | argsNum == 0 -> def
          Just (InlinePartiallyApplied 0) -> def
          Just InlineAlways -> def
          Just InlineNever -> node
          _
            | HashSet.member _identSymbol nonRecSyms
                && ( HashMap.lookup _identSymbol symOcc == Just 1
                       || isImmediate md def
                   ) ->
                def
            | otherwise ->
                node
        where
          ii = lookupIdentifierInfo md _identSymbol
          pi = ii ^. identifierPragmas . pragmasInline
          argsNum = ii ^. identifierArgsNum
          def = lookupIdentifierNode md _identSymbol
      -- inline zero-argument definitions (automatically) if inlining would result
      -- in case reduction
      NCase cs@Case {..} ->
        let (h, args) = unfoldApps _caseValue
         in case h of
              NIdt Ident {..} -> case pi of
                Just InlineCase ->
                  NCase cs {_caseValue = mkApps def args}
                Just InlineNever ->
                  node
                Nothing
                  | HashSet.member _identSymbol nonRecSyms
                      && checkLambdaConstructorApp (length args) bl def ->
                      NCase cs {_caseValue = mkApps def args}
                _ ->
                  node
                where
                  ii = lookupIdentifierInfo md _identSymbol
                  pi = ii ^. identifierPragmas . pragmasInline
                  def = lookupIdentifierNode md _identSymbol
              _ ->
                node
      _ ->
        node

    checkLambdaConstructorApp :: Int -> BinderList Binder -> Node -> Bool
    checkLambdaConstructorApp argsNum bl node =
      argsNum >= lamsNum && isConstructorApp body && checkDepth md bl inlineDepth body
      where
        (lamsNum, body) = unfoldLambdas' node

inlining' :: Int -> HashSet Symbol -> HashMap Symbol Int -> Module -> Module
inlining' inliningDepth nonRecSyms symOcc md = mapT (const (convertNode inliningDepth nonRecSyms symOcc md)) md

inlining :: (Member (Reader CoreOptions) r) => Module -> Sem r Module
inlining md = do
  d <- asks (^. optInliningDepth)
  return $ inlining' d (nonRecursiveIdents md) (getModuleSymbolsMap md) md
