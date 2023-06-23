module Juvix.Compiler.Core.Transformation.Optimize.SpecializeArgs where

import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Compiler.Core.Data.InfoTableBuilder
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Transformation.LambdaLetRecLifting (lambdaLiftNode')

isSpecializable :: Node -> Bool
isSpecializable = \case
  NIdt {} -> True
  NLam {} -> True
  NApp {} -> False
  _ -> False

isArgSpecializable :: InfoTable -> Symbol -> Int -> Bool
isArgSpecializable tab sym argNum = run $ execState True $ dmapNRM go body
  where
    nodeSym = lookupIdentifierNode tab sym
    (lams, body) = unfoldLambdas nodeSym
    n = length lams

    go :: Member (State Bool) r => Level -> Node -> Sem r Recur
    go lvl node = case node of
      NApp {} ->
        let (h, args) = unfoldApps' node
         in case h of
              NIdt Ident {..}
                | _identSymbol == sym ->
                    let b =
                          argNum <= length args
                            && case args !! (argNum - 1) of
                              NVar Var {..} | _varIndex == lvl + n - argNum -> True
                              _ -> False
                     in do
                          modify' (&& b)
                          mapM_ (dmapNRM' (lvl, go)) args
                          return $ End node
              _ -> return $ Recur node
      NIdt Ident {..}
        | _identSymbol == sym -> do
            put False
            return $ End node
      _ -> return $ Recur node

convertNode :: forall r. Member InfoTableBuilder r => InfoTable -> Node -> Sem r Node
convertNode tab = dmapLRM go
  where
    go :: BinderList Binder -> Node -> Sem r Recur
    go bl node = case node of
      NApp {} ->
        let (h, args) = unfoldApps' node
         in case h of
              NIdt Ident {..} ->
                case pspec of
                  Just PragmaSpecialiseArgs {..} -> do
                    args' <- mapM (dmapLRM' (bl, go)) args
                    -- assumption: all type variables are at the front
                    let specargs0 =
                          filter
                            ( \argNum ->
                                argNum <= argsNum
                                  && argNum <= length args'
                                  && isSpecializable (args' !! (argNum - 1))
                                  && isArgSpecializable tab _identSymbol argNum
                            )
                            _pragmaSpecialiseArgs
                        -- in addition to the arguments explicitly marked for
                        -- specialisation, also specialise all type arguments
                        specargs =
                          nub $
                            [1 .. length (takeWhile (isTypeConstr tab) tyargs)]
                              ++ specargs0
                    if
                        | null specargs0 ->
                            return $ End (mkApps' h args')
                        | otherwise -> do
                            let def = lookupIdentifierNode tab _identSymbol
                                (lams, body) = unfoldLambdas def
                                body' =
                                  replaceIdent _identSymbol argsNum specargs $
                                    replaceArgs argsNum specargs args' body
                                tyargs' = removeSpecTypeArgs specargs args' tyargs
                                -- `replaceArgs` accounts for the `letrec`
                                -- binding added, so we need to shift by -1
                                -- since the type is not in scope of the letrec
                                -- binding
                                tgt' = shift (-1) (replaceArgs argsNum specargs args' tgt)
                                lams' =
                                  zipWith
                                    (\lam ty -> over lambdaLhsBinder (set binderType ty) lam)
                                    (removeSpecargs specargs lams)
                                    tyargs'
                                args'' = removeSpecargs specargs args'
                                letitem =
                                  mkLetItem
                                    (ii ^. identifierName)
                                    (mkPis' tyargs' tgt')
                                    (reLambdas lams' body')
                                node' =
                                  mkLetRec
                                    mempty
                                    (NonEmpty.singleton letitem)
                                    (mkApps' (mkVar' 0) args'')
                            node'' <- lambdaLiftNode' True bl node'
                            return $ End node''
                  Nothing ->
                    return $ Recur node
                where
                  ii = lookupIdentifierInfo tab _identSymbol
                  pspec = ii ^. identifierPragmas . pragmasSpecialiseArgs
                  argsNum = ii ^. identifierArgsNum
                  (tyargs, tgt) = unfoldPi' (ii ^. identifierType)
              _ ->
                return $ Recur node
      _ ->
        return $ Recur node

    removeSpecTypeArgs :: [Int] -> [Node] -> [Type] -> [Type]
    removeSpecTypeArgs = goRemove 1
      where
        goRemove :: Int -> [Int] -> [Node] -> [Type] -> [Type]
        goRemove n specargs args tys = case (tys, args) of
          ([], []) -> []
          (ty : tys', arg : args')
            | n `elem` specargs ->
                goRemove (n + 1) specargs args' (substDrop [arg] tys)
            | otherwise ->
                ty : goRemove (n + 1) specargs (map (shift 1) args') tys'
          _ -> impossible

    removeSpecargs :: [Int] -> [a] -> [a]
    removeSpecargs specargs args =
      map fst $
        filter
          (not . (`elem` specargs) . snd)
          (zip args [1 ..])

    -- replace the calls to the function being specialised with the specialised
    -- version (omitting the specialised arguments)
    replaceIdent :: Symbol -> Int -> [Int] -> Node -> Node
    replaceIdent sym argsNum specargs = dmapNR goReplace
      where
        argsNum' = argsNum - length specargs

        goReplace :: Level -> Node -> Recur
        goReplace lvl node = case node of
          NApp {} ->
            let (h, args) = unfoldApps' node
             in case h of
                  NIdt Ident {..}
                    | _identSymbol == sym ->
                        let args' =
                              map
                                (replaceIdent sym argsNum specargs)
                                (removeSpecargs specargs args)
                         in End $ mkApps' (mkVar' (lvl + argsNum')) args'
                  _ ->
                    Recur node
          _ ->
            Recur node

    -- replace the arguments being specialised with the actual argument values
    replaceArgs :: Int -> [Int] -> [Node] -> Node -> Node
    replaceArgs argsNum specargs args = umapN goReplace
      where
        argsNum' = argsNum - length specargs

        goReplace :: Level -> Node -> Node
        goReplace lvl node = case node of
          NVar v@Var {..}
            | _varIndex >= lvl ->
                if
                    | argIdx < argsNum ->
                        if
                            | argNum `elem` specargs ->
                                -- paste in the argument we specialise by
                                shift (lvl + argsNum' + 1) (args !! (argNum - 1))
                            | otherwise ->
                                -- decrease de Bruijn index by the number of lambdas removed below the binder
                                NVar $
                                  shiftVar (-(length (filter (argNum <) specargs))) v
                    | otherwise ->
                        -- (argsNum - argsNum') binders removed (the specialised arguments) and 1 binder added (the letrec binder)
                        NVar $ shiftVar (argsNum' - argsNum + 1) v
            where
              argIdx = _varIndex - lvl
              argNum = argsNum - argIdx
          _ -> node

specializeArgs :: InfoTable -> InfoTable
specializeArgs tab = run $ mapT' (const (convertNode tab)) tab
