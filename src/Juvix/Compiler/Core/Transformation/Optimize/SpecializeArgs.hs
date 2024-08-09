module Juvix.Compiler.Core.Transformation.Optimize.SpecializeArgs where

import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Compiler.Core.Data.InfoTableBuilder
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info.PragmaInfo
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Transformation.LambdaLetRecLifting (lambdaLiftNode')

-- | Check if an argument value is suitable for specialisation (e.g. not a
-- variable)
isSpecializable :: Module -> Node -> Bool
isSpecializable md node =
  isType md mempty node
    || case node of
      NIdt Ident {..} ->
        case lookupIdentifierInfo md _identSymbol ^. identifierPragmas . pragmasSpecialise of
          Just (PragmaSpecialise False) -> False
          _ -> True
      NLam {} -> True
      NCst {} -> True
      NCtr Constr {..} ->
        -- TODO: rethink this
        all (isSpecializable md) _constrArgs
      NApp {} ->
        let (h, _) = unfoldApps' node
         in isSpecializable md h
      _ -> False

-- | Check for `h a1 .. an` where `h` is an identifier explicitly marked for
-- specialisation with `specialize: true`.
isMarkedSpecializable :: Module -> Node -> Bool
isMarkedSpecializable md = \case
  NTyp TypeConstr {..}
    | Just (PragmaSpecialise True) <-
        lookupInductiveInfo md _typeConstrSymbol
          ^. inductivePragmas . pragmasSpecialise ->
        True
  node ->
    let (h, _) = unfoldApps' node
     in case h of
          NIdt Ident {..}
            | Just (PragmaSpecialise True) <-
                lookupIdentifierInfo md _identSymbol
                  ^. identifierPragmas . pragmasSpecialise ->
                True
          _ ->
            False

-- | Checks if an argument is passed without modification to recursive calls.
isArgSpecializable :: Module -> Symbol -> Int -> Bool
isArgSpecializable tab sym argNum = run $ execState True $ dmapNRM go body
  where
    nodeSym = lookupIdentifierNode tab sym
    (lams, body) = unfoldLambdas nodeSym
    n = length lams

    go :: (Member (State Bool) r) => Level -> Node -> Sem r Recur
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

convertNode :: forall r. (Member InfoTableBuilder r) => Node -> Sem r Node
convertNode = dmapLRM go
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
    goIdentApp bl idt@Ident {..} args = do
      args' <- mapM (dmapLRM' (bl, go)) args
      md <- getModule
      let ii = lookupIdentifierInfo md _identSymbol
          pspec = ii ^. identifierPragmas . pragmasSpecialiseArgs
          pspecby = ii ^. identifierPragmas . pragmasSpecialiseBy
          argsNum = ii ^. identifierArgsNum
          (tyargs, tgt) = unfoldPi' (ii ^. identifierType)
          def = lookupIdentifierNode md _identSymbol
          (lams, body) = unfoldLambdas def
          argnames =
            zipWith
              (\mn lhs -> fromMaybe (lhs ^. lambdaLhsBinder . binderName) mn)
              (ii ^. identifierArgNames)
              lams

          -- arguments marked for specialisation with `specialize: true`
          psargs0 =
            map fst3 $
              filter (\(_, arg, ty) -> isMarkedSpecializable md arg || isMarkedSpecializable md ty) $
                zip3 [1 .. argsNum] args' tyargs

          getArgIndex :: PragmaSpecialiseArg -> Maybe Int
          getArgIndex = \case
            SpecialiseArgNum i -> Just i
            SpecialiseArgNamed x -> fmap (+ 1) $ x `elemIndex` argnames
      if
          | (isJust pspec || isJust pspecby || not (null psargs0)) && length args == argsNum -> do
              let psargs1 = mapMaybe getArgIndex $ maybe [] (^. pragmaSpecialiseArgs) pspec
                  psargs2 = maybe [] (map (+ 1) . mapMaybe (`elemIndex` argnames) . (^. pragmaSpecialiseBy)) pspecby
                  -- psargs are the arguments explicitly marked for specialization
                  psargs = nubSort (psargs0 ++ psargs1 ++ psargs2)
              let -- specargs0 are the arguments actually selected for specialization
                  specargs0 =
                    filter
                      ( \argNum ->
                          argNum <= argsNum
                            && isSpecializable md (args' !! (argNum - 1))
                            && isArgSpecializable md _identSymbol argNum
                      )
                      psargs
                  tyargnums = map fst $ filter (isTypeConstr md . snd) $ zip [1 .. argsNum] tyargs
                  -- in addition to the arguments explicitly marked for
                  -- specialisation, also specialise all type arguments
                  specargs = nub $ tyargnums ++ specargs0
                  -- the arguments marked for specialisation which we don't
                  -- specialise now
                  remainingSpecargs =
                    shiftSpecargs specargs $ filter (not . (`elem` specargs0)) psargs
                  pragmas =
                    (ii ^. identifierPragmas)
                      { _pragmasSpecialiseArgs =
                          Just $
                            PragmaSpecialiseArgs $
                              map SpecialiseArgNum remainingSpecargs
                      }

                  createFun :: Int -> Symbol -> [Node] -> (Type, [LambdaLhs], Node)
                  createFun shiftIdx sym' sargs =
                    let body' =
                          replaceArgs shiftIdx argsNum specargs sargs $
                            replaceIdent _identSymbol sym' argsNum specargs body
                        tyargs' = removeSpecTypeArgs specargs sargs (take argsNum tyargs)
                        tgt' = replaceArgs shiftIdx argsNum specargs sargs (mkPis' (drop argsNum tyargs) tgt)
                        ty' = mkPis' tyargs' tgt'
                        lams' =
                          zipWithExact
                            (\lam ty -> over lambdaLhsBinder (set binderType ty) lam)
                            (removeSpecargs specargs lams)
                            tyargs'
                     in (ty', lams', body')
              if
                  | null specargs0 ->
                      return $ End (mkApps' (NIdt idt) args')
                  | otherwise -> do
                      massert (length lams == argsNum)
                      massert (length args' == argsNum)
                      massert (argsNum <= length tyargs)
                      -- the specialisation signature: the values we specialise the arguments by
                      let specSigArgs = selectSpecargs specargs args'
                          specSig = (specSigArgs, specargs)
                      if
                          | all isClosed specSigArgs ->
                              case find ((== specSig) . (^. specSignature)) (lookupSpecialisationInfo md _identSymbol) of
                                Just SpecialisationInfo {..} ->
                                  return $
                                    End $
                                      mkApps'
                                        (mkIdent' _specSymbol)
                                        (removeSpecargs specargs args')
                                Nothing -> do
                                  sym' <- freshSymbol
                                  let (ty', lams', body') = createFun 0 sym' args'
                                      name = uniqueName ("spec_" <> ii ^. identifierName) sym'
                                  registerIdent
                                    name
                                    IdentifierInfo
                                      { _identifierSymbol = sym',
                                        _identifierName = name,
                                        _identifierLocation = ii ^. identifierLocation,
                                        _identifierType = ty',
                                        _identifierArgsNum = length lams',
                                        _identifierIsExported = False,
                                        _identifierBuiltin = Nothing,
                                        _identifierPragmas = pragmas,
                                        _identifierArgNames =
                                          removeSpecargs specargs (ii ^. identifierArgNames)
                                      }
                                  registerIdentNode sym' (reLambdas lams' body')
                                  let si =
                                        SpecialisationInfo
                                          { _specSignature = specSig,
                                            _specSymbol = sym'
                                          }
                                  registerSpecialisation _identSymbol si
                                  return $
                                    End $
                                      mkApps'
                                        (mkIdent' sym')
                                        (removeSpecargs specargs args')
                          | otherwise -> do
                              sym' <- freshSymbol
                              let -- We're adding the letrec binder, so need to shift by 1
                                  sargs = map (shift 1) args'
                                  (ty', lams', body') = createFun 1 sym' sargs
                                  body'' = substSym sym' (argsNum - length specargs) body'
                                  args'' = removeSpecargs specargs sargs
                                  fun = reLambdas lams' body''
                                  letitem =
                                    mkLetItem
                                      (ii ^. identifierName)
                                      -- the type is not in the scope of the binder
                                      (shift (-1) ty')
                                      fun
                                  node' =
                                    mkLetRec
                                      (setInfoPragmas [pragmas] mempty)
                                      (NonEmpty.singleton letitem)
                                      (mkApps' (mkVar' 0) args'')
                              node'' <- lambdaLiftNode' True bl node'
                              return $ End node''
          | otherwise ->
              return $ End $ mkApps' (NIdt idt) args'

    -- Because all type arguments are substituted (specialized), in the end no
    -- binders in the resulting type list refer to other elements in the list
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

    selectSpecargs :: [Int] -> [a] -> [a]
    selectSpecargs specargs args =
      map fst $
        filter
          ((`elem` specargs) . snd)
          (zip args [1 ..])

    shiftSpecargs :: [Int] -> [Int] -> [Int]
    shiftSpecargs specargs =
      map (\argNum -> argNum - length (filter (argNum >) specargs))

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
    replaceArgs :: Int -> Int -> [Int] -> [Node] -> Node -> Node
    replaceArgs shiftIdx argsNum specargs args = umapN goReplace
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
                        -- (argsNum - argsNum') binders removed (the specialised arguments) and shiftIdx binders added (the letrec binders)
                        NVar $ shiftVar (argsNum' - argsNum + shiftIdx) v
            where
              argIdx = _varIndex - lvl
              argNum = argsNum - argIdx
          _ -> node

specializeArgs :: Module -> Module
specializeArgs = run . mapT' (const convertNode)
