module Juvix.Compiler.Core.Transformation.LambdaLifting
  ( module Juvix.Compiler.Core.Transformation.LambdaLifting,
    module Juvix.Compiler.Core.Transformation.Base,
  )
where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Juvix.Compiler.Core.Data.BinderList (BinderList)
import Juvix.Compiler.Core.Data.BinderList qualified as BL
import Juvix.Compiler.Core.Data.InfoTableBuilder
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Pretty
import Juvix.Compiler.Core.Transformation.Base

lambdaLiftNode :: forall r. Member InfoTableBuilder r => BinderList Binder -> Node -> Sem r Node
lambdaLiftNode aboveBl top =
  reLambdas topArgs <$> dmapLRM' (topArgsBinderList <> aboveBl, go) body
  where
    (topArgs, body) = unfoldLambdas top
    topArgsBinderList :: BinderList Binder
    topArgsBinderList = BL.fromList (map (^. lambdaLhsBinder) topArgs)
    typeFromArgs :: [ArgumentInfo] -> Type
    typeFromArgs = \case
      [] -> mkDynamic' -- change this when we have type info about the body
      (a : as) -> mkPi' (a ^. argumentType) (typeFromArgs as)
    -- extracts the argument info from the binder
    go :: BinderList Binder -> Node -> Sem r Recur
    go bl = \case
      NLam l -> goLambda l
      NRec l -> goLetRec l
      m -> return (Recur m)
      where
        captureFreeVars' :: Node -> Set Var -> Sem r ([Var], Node)
        captureFreeVars' n m = do
          runOutputList $ goCapture bl 0 0 n m
          where
            goCapture ::
              BinderList Binder ->
              Index ->
              Index ->
              Node ->
              Set Var ->
              Sem (Output Var ': r) Node
            goCapture ctx lastidx offset body' fv = case Set.minView fv of
              Nothing -> return body'
              Just (v, vs) -> do
                let idx = v ^. varIndex
                    bi = BL.lookup' idx ctx
                    -- the number of skipped binders
                    skipped = idx - lastidx + 1
                -- traceM ("k: " <> prettyText k)
                let ctx' = BL.drop' skipped ctx
                bi' <- traverseOf binderType (lambdaLiftNode ctx') bi
                let freevarsbi' = freeVarsSorted (bi' ^. binderType)
                    vs' :: Set Var
                    vs' = Set.mapMonotonic (over varIndex (\y -> y - skipped)) vs
                output (shiftVar offset v)
                goCapture
                  ctx'
                  idx
                  (offset + skipped)
                  (mkLambdaB bi' (shift (-skipped + 1) body'))
                  (freevarsbi' <> vs')

        goLambda :: Lambda -> Sem r Recur
        goLambda lm = do
          l' <- lambdaLiftNode (BL.extend (lm ^. lambdaBinder) bl) (NLam lm)
          let freevars = freeVarsSorted l'
          -- forM_ freevars $ \fv -> traceM (ppTrace (NVar fv))
          (allfreevars, fBody') <- captureFreeVars' l' freevars
          let freevarsAssocs :: Map Index Binder
              freevarsAssocs = Map.fromList [(i, BL.lookup' i bl) | i <- map (^. varIndex) allfreevars]
          let argsInfo :: [ArgumentInfo]
              argsInfo = map (argumentInfoFromBinder . snd) (Map.toList freevarsAssocs)
          f <- freshSymbol
          registerIdent
            IdentifierInfo
              { _identifierSymbol = f,
                _identifierName = Nothing,
                _identifierType = typeFromArgs argsInfo,
                _identifierArgsNum = length freevars,
                _identifierArgsInfo = argsInfo,
                _identifierIsExported = False
              }
          registerIdentNode f fBody'
          let fApp = mkApps' (mkIdent mempty f) (map NVar allfreevars)
          return (End fApp)

        goLetRec :: LetRec -> Sem r Recur
        goLetRec letr = do
          let defs :: [Node]
              defs = toList (letr ^.. letRecValues . each . letItemValue)
              ndefs :: Int
              ndefs = length defs
              letRecBinders :: [Binder]
              letRecBinders = letr ^.. letRecValues . each . letItemBinder
              bl' :: BinderList Binder
              bl' = BL.prepend letRecBinders bl
          topSyms :: [Symbol] <- forM defs (const freshSymbol)
          let recItemsFreeVars :: [(Var, Binder)]
              recItemsFreeVars = mapMaybe helper (toList (mconcatMap freeVarsSet defs))
                where
                  -- free vars in each let
                  -- throw away variables bound in the letrec and shift others
                  helper :: Var -> Maybe (Var, Binder)
                  helper v
                    | v ^. varIndex < ndefs = Nothing
                    | otherwise = Just (set varIndex idx' v, BL.lookup idx' bl)
                    where
                      idx' = (v ^. varIndex) - ndefs

              subsCalls :: Node -> Node
              subsCalls =
                substs
                  ( reverse
                      [ mkApps' (mkIdent' sym) (map (NVar . fst) recItemsFreeVars)
                        | sym <- topSyms
                      ]
                  )
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
                          argsInfo = map (argumentInfoFromBinder . snd) recItemsFreeVars
                      registerIdentNode sym topBody
                      registerIdent
                        IdentifierInfo
                          { _identifierSymbol = sym,
                            _identifierName = itemBinder ^. binderName,
                            _identifierType = typeFromArgs argsInfo,
                            _identifierArgsNum = length recItemsFreeVars,
                            _identifierArgsInfo = argsInfo,
                            _identifierIsExported = False
                          }
                    | (sym, (itemBinder, b)) <- zipExact topSyms (zipExact letRecBinders liftedDefs)
                  ]
              letItems :: [Node]
              letItems =
                let fv = recItemsFreeVars
                 in [ mkApps' (mkIdent' s) (map (NVar . fst) fv)
                      | s <- topSyms
                    ]
          declareTopSyms

          let -- TODO it can probably be simplified
              shiftHelper :: Node -> NonEmpty Node -> Node
              shiftHelper b = goShift 0
                where
                  goShift :: Int -> NonEmpty Node -> Node
                  goShift k = \case
                    x :| yys -> case yys of
                      []
                        | k == ndefs - 1 -> mkLet' (shift k x) b
                        | otherwise -> impossible
                      (y : ys) -> mkLet' (shift k x) (goShift (k + 1) (y :| ys))
          let res :: Node
              res = shiftHelper body' (nonEmpty' letItems)
          return (Recur res)

lambdaLifting :: InfoTable -> InfoTable
lambdaLifting = run . mapT' (lambdaLiftNode mempty)

-- | True if lambdas are only found at the top level
nodeIsLifted :: Node -> Bool
nodeIsLifted = not . hasNestedLambdas
  where
    hasNestedLambdas :: Node -> Bool
    hasNestedLambdas = has (cosmos . _NLam) . snd . unfoldLambdas'

isLifted :: InfoTable -> Bool
isLifted = all nodeIsLifted . toList . (^. identContext)
