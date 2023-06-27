module Juvix.Compiler.Core.Transformation.Optimize.SpecializeArgs where

import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Compiler.Core.Data.InfoTableBuilder
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Transformation.LambdaLetRecLifting (lambdaLiftNode')

isSpecializable :: InfoTable -> Node -> Bool
isSpecializable tab node =
  isTypeConstr tab node
    || case node of
      NIdt {} -> True
      NLam {} -> True
      NCst {} -> True
      NCtr Constr {..} -> all (isSpecializable tab) _constrArgs
      NApp {} ->
        let (h, _) = unfoldApps' node
         in isSpecializable tab h
      _ -> False

-- | Checks if an argument is passed without modification to recursive calls.
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
              NIdt idt ->
                goIdentApp bl idt args
              _ ->
                return $ Recur node
      _ ->
        return $ Recur node

    goIdentApp :: BinderList Binder -> Ident -> [Node] -> Sem r Recur
    goIdentApp bl idt@Ident {..} args =
      case pspec of
        Just PragmaSpecialiseArgs {..} | length args == argsNum -> do
          args' <- mapM (dmapLRM' (bl, go)) args
          -- assumption: all type variables are at the front
          let specargs0 =
                filter
                  ( \argNum ->
                      argNum <= argsNum
                        && argNum <= length args'
                        && isSpecializable tab (args' !! (argNum - 1))
                        && isArgSpecializable tab _identSymbol argNum
                  )
                  _pragmaSpecialiseArgs
              tyargsNum = length (takeWhile (isTypeConstr tab) tyargs)
              -- in addition to the arguments explicitly marked for
              -- specialisation, also specialise all type arguments
              specargs =
                nub $
                  [1 .. tyargsNum]
                    ++ specargs0
          if
              | null specargs0 ->
                  return $ End (mkApps' (NIdt idt) args')
              | otherwise -> do
                  eassert (tyargsNum < argsNum)
                  let def = lookupIdentifierNode tab _identSymbol
                      (lams, body) = unfoldLambdas def
                  eassert (length lams == argsNum)
                  eassert (length args' == argsNum)
                  eassert (argsNum <= length tyargs)
                  sym' <- freshSymbol
                  let -- We're adding the letrec binder, so need to shift by 1
                      sargs = map (shift 1) args'
                      body' =
                        substSym sym' (argsNum - length specargs) $
                          replaceArgs argsNum specargs sargs $
                            replaceIdent _identSymbol sym' argsNum specargs body
                      tyargs' = removeSpecTypeArgs specargs sargs (take argsNum tyargs)
                      tgt' = replaceArgs argsNum specargs sargs (mkPis' (drop argsNum tyargs) tgt)
                      ty' = mkPis' tyargs' tgt'
                      lams' =
                        zipWithExact
                          (\lam ty -> over lambdaLhsBinder (set binderType ty) lam)
                          (removeSpecargs specargs lams)
                          tyargs'
                      args'' = removeSpecargs specargs sargs
                      letitem =
                        mkLetItem
                          (ii ^. identifierName)
                          -- the type is not in the scope of the binder
                          (shift (-1) ty')
                          (reLambdas lams' body')
                      node' =
                        mkLetRec
                          mempty
                          (NonEmpty.singleton letitem)
                          (mkApps' (mkVar' 0) args'')
                  node'' <- lambdaLiftNode' True bl node'
                  return $ End node''
        _ ->
          return $ Recur $ mkApps' (NIdt idt) args
      where
        ii = lookupIdentifierInfo tab _identSymbol
        pspec = ii ^. identifierPragmas . pragmasSpecialiseArgs
        argsNum = ii ^. identifierArgsNum
        (tyargs, tgt) = unfoldPi' (ii ^. identifierType)

    -- assumption: all type arguments are substituted, so no binders in the type
    -- list refer to other elements in the list
    removeSpecTypeArgs :: [Int] -> [Node] -> [Type] -> [Type]
    removeSpecTypeArgs = goRemove 1
      where
        goRemove :: Int -> [Int] -> [Node] -> [Type] -> [Type]
        goRemove n specargs args tys = case (tys, args) of
          ([], []) -> []
          (ty : tys', arg : args')
            | n `elem` specargs ->
                let tys'' = zipWith (\ty' k -> substVar k (shift k arg) ty') tys' [0 ..]
                 in goRemove (n + 1) specargs args' tys''
            | otherwise ->
                ty : goRemove (n + 1) specargs (map (shift 1) args') tys'
          _ -> impossible

    removeSpecargs :: [Int] -> [a] -> [a]
    removeSpecargs specargs args =
      map fst $
        filter
          (not . (`elem` specargs) . snd)
          (zip args [1 ..])

    -- Replace the calls to the function being specialised with the specialised
    -- version (omitting the specialised arguments). We need to first replace
    -- with a fresh identifier, and only substitute the variable after replacing
    -- the arguments, to avoid erroneous substitution when one of the
    -- specialized arguments refers to the function being specialized, e.g., `map
    -- (map f)`.
    replaceIdent :: Symbol -> Symbol -> Int -> [Int] -> Node -> Node
    replaceIdent sym sym' argsNum specargs = dmapR goReplace
      where
        goReplace :: Node -> Recur
        goReplace node = case node of
          NApp {} ->
            let (h, args) = unfoldApps' node
             in case h of
                  NIdt Ident {..}
                    | _identSymbol == sym ->
                        let args' =
                              map
                                (replaceIdent sym sym' argsNum specargs)
                                (removeSpecargs specargs args)
                         in End $ mkApps' (mkIdent' sym') args'
                  _ ->
                    Recur node
          _ ->
            Recur node

    substSym :: Symbol -> Int -> Node -> Node
    substSym sym k = umapN goSubst
      where
        goSubst :: Level -> Node -> Node
        goSubst lvl = \case
          NIdt Ident {..}
            | _identSymbol == sym ->
                mkVar' (lvl + k)
          node ->
            node

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
                                shift (lvl + argsNum') (args !! (argNum - 1))
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
