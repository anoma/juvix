module Juvix.Compiler.Core.Transformation.LambdaLetRecLifting
  ( module Juvix.Compiler.Core.Transformation.LambdaLetRecLifting,
    module Juvix.Compiler.Core.Transformation.Base,
  )
where

import Juvix.Compiler.Core.Data.BinderList qualified as BL
import Juvix.Compiler.Core.Data.InfoTableBuilder
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info.NameInfo
import Juvix.Compiler.Core.Pretty
import Juvix.Compiler.Core.Transformation.Base

lambdaLiftBinder :: (Member InfoTableBuilder r) => BinderList Binder -> Binder -> Sem r Binder
lambdaLiftBinder bl = traverseOf binderType (lambdaLiftNode bl)

lambdaLiftNode :: forall r. (Member InfoTableBuilder r) => BinderList Binder -> Node -> Sem r Node
lambdaLiftNode aboveBl top =
  let topArgs :: [LambdaLhs]
      (topArgs, body) = unfoldLambdas top
   in goTop aboveBl body topArgs
  where
    typeFromArgs :: [ArgumentInfo] -> Type
    typeFromArgs = \case
      [] -> mkDynamic' -- change this when we have type info about the body
      (a : as) -> mkPi' (a ^. argumentType) (typeFromArgs as)

    goTop :: BinderList Binder -> Node -> [LambdaLhs] -> Sem r Node
    goTop bl body = \case
      [] -> dmapLRM' (bl, go) body
      l : ls -> do
        l' <- traverseOf lambdaLhsBinder (lambdaLiftBinder bl) l
        reLambda l' <$> goTop (BL.cons (l' ^. lambdaLhsBinder) bl) body ls

    -- extracts the argument info from the binder
    go :: BinderList Binder -> Node -> Sem r Recur
    go bl = \case
      NLam l -> goLambda l
      NRec l -> goLetRec l
      m -> return (Recur m)
      where
        goLambda :: Lambda -> Sem r Recur
        goLambda lm = do
          l' <- lambdaLiftNode bl (NLam lm)
          let (freevarsAssocs, fBody') = captureFreeVarsCtx bl l'
              allfreevars :: [Var]
              allfreevars = map fst freevarsAssocs
              argsInfo :: [ArgumentInfo]
              argsInfo = map (argumentInfoFromBinder . (^. lambdaLhsBinder)) (fst (unfoldLambdas fBody'))
          f <- freshSymbol
          let name = uniqueName "lambda" f
          registerIdent
            name
            IdentifierInfo
              { _identifierSymbol = f,
                _identifierName = name,
                _identifierLocation = Nothing,
                _identifierType = typeFromArgs argsInfo,
                _identifierArgsNum = length argsInfo,
                _identifierArgsInfo = argsInfo,
                _identifierIsExported = False,
                _identifierBuiltin = Nothing
              }
          registerIdentNode f fBody'
          let fApp = mkApps' (mkIdent (setInfoName name mempty) f) (map NVar allfreevars)
          return (End fApp)

        goLetRec :: LetRec -> Sem r Recur
        goLetRec letr = do
          let defs :: [Node]
              defs = toList (letr ^.. letRecValues . each . letItemValue)
              ndefs :: Int
              ndefs = length defs
          letRecBinders' :: [Binder] <- mapM (lambdaLiftBinder bl) (letr ^.. letRecValues . each . letItemBinder)
          let bl' :: BinderList Binder
              -- the reverse is necessary because the last item in letRecBinders has index 0
              bl' = BL.prependRev letRecBinders' bl
          topSyms :: [Symbol] <- forM defs (const freshSymbol)

          let topNames :: [Text]
              topNames = zipWithExact uniqueName (map (^. binderName) letRecBinders') topSyms
              topSymsWithName = zipExact topSyms topNames

          let recItemsFreeVars :: [(Var, Binder)]
              recItemsFreeVars = mapMaybe helper (concatMap (freeVarsCtx' bl') defs)
                where
                  helper :: Var -> Maybe (Var, Binder)
                  helper v
                    | v ^. varIndex < ndefs = Nothing
                    | otherwise = Just (set varIndex idx' v, BL.lookup idx' bl)
                    where
                      idx' = (v ^. varIndex) - ndefs

              letItems :: [Node]
              letItems =
                [ mkApps' (mkIdent (setInfoName name mempty) sym) (map (NVar . fst) recItemsFreeVars)
                  | (sym, name) <- topSymsWithName
                ]

              subsCalls :: Node -> Node
              subsCalls = substs (reverse letItems)
          -- NOTE that we are first substituting the calls and then performing
          -- lambda lifting. This is a tradeoff. We have slower compilation but
          -- slightly faster execution time, since it minimizes the number of
          -- free variables that need to be passed around.
          liftedDefs <- mapM (lambdaLiftNode bl . subsCalls) defs
          body' <- lambdaLiftNode bl' (letr ^. letRecBody)
          let declareTopSyms :: Sem r ()
              declareTopSyms =
                sequence_
                  [ do
                      let topBody = captureFreeVars (map (first (^. varIndex)) recItemsFreeVars) b
                          argsInfo :: [ArgumentInfo]
                          argsInfo =
                            map (argumentInfoFromBinder . (^. lambdaLhsBinder)) (fst (unfoldLambdas topBody))
                      registerIdentNode sym topBody
                      registerIdent
                        name
                        IdentifierInfo
                          { _identifierSymbol = sym,
                            _identifierName = name,
                            _identifierLocation = itemBinder ^. binderLocation,
                            _identifierType = typeFromArgs argsInfo,
                            _identifierArgsNum = length argsInfo,
                            _identifierArgsInfo = argsInfo,
                            _identifierIsExported = False,
                            _identifierBuiltin = Nothing
                          }
                    | ((sym, name), (itemBinder, b)) <- zipExact topSymsWithName (zipExact letRecBinders' liftedDefs)
                  ]
          declareTopSyms

          let -- TODO it can probably be simplified
              shiftHelper :: Node -> NonEmpty (Node, Binder) -> Node
              shiftHelper b = goShift 0
                where
                  goShift :: Int -> NonEmpty (Node, Binder) -> Node
                  goShift k = \case
                    (x, bnd) :| yys -> case yys of
                      []
                        | k == ndefs - 1 -> mkLet mempty bnd' (shift k x) b
                        | otherwise -> impossible
                      (y : ys) -> mkLet mempty bnd' (shift k x) (goShift (k + 1) (y :| ys))
                      where
                        bnd' = over binderType (shift k . subsCalls . shift (-ndefs)) bnd
          -- TODO: the types should also be lambda-lifted
          let res :: Node
              res = shiftHelper body' (nonEmpty' (zipExact letItems letRecBinders'))
          return (Recur res)

lambdaLifting :: InfoTable -> InfoTable
lambdaLifting = run . mapT' (const (lambdaLiftNode mempty))

-- | True if lambdas are only found at the top level
nodeIsLifted :: Node -> Bool
nodeIsLifted = not . hasNestedLambdas
  where
    hasNestedLambdas :: Node -> Bool
    hasNestedLambdas = has (cosmos . _NLam) . snd . unfoldLambdas'

isLifted :: InfoTable -> Bool
isLifted = all nodeIsLifted . toList . (^. identContext)
