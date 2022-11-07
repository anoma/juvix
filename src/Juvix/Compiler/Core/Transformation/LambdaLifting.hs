module Juvix.Compiler.Core.Transformation.LambdaLifting
  ( module Juvix.Compiler.Core.Transformation.LambdaLifting,
    module Juvix.Compiler.Core.Transformation.Base,
  )
where

import Data.Set qualified as Set
import Juvix.Compiler.Core.Data.BinderList (BinderList)
import Juvix.Compiler.Core.Data.BinderList qualified as BL
import Juvix.Compiler.Core.Data.InfoTableBuilder
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Pretty
import Juvix.Compiler.Core.Transformation.Base

-- | all free variables, including those appearing in binders of other free variables
freeVarsAll :: BinderList Binder -> Node -> [(Var, Binder)]
freeVarsAll ctx = fst . captureFreeVars2 ctx

freeVarsAll' :: BinderList Binder -> Node -> [Var]
freeVarsAll' ctx = map fst . freeVarsAll ctx

-- captures all free variables of a node. It also returns the list of captured
-- variables in left-to-right order: if snd is of the form λxλy... then fst is
-- [x, y]
captureFreeVars2 :: BinderList Binder -> Node -> ([(Var, Binder)], Node)
captureFreeVars2 bl n =
  first (BL.lookupsSorted bl . reverse) . run . runOutputList $ goCapture bl 0 n (freeVarsSorted n)
  where
    goCapture ::
      BinderList Binder ->
      Index ->
      Node ->
      Set Var ->
      Sem '[Output Var] Node
    goCapture ctx offset body' fv = case Set.minView fv of
      Nothing -> return body'
      Just (v, vs) -> do
        let idx = v ^. varIndex
            bi = BL.lookup idx ctx
            -- the number of consumed binders
            consumed = idx + 1
            ctx' = BL.drop' consumed ctx
        let freevarsbi' = freeVarsSorted (bi ^. binderType)
            -- shifting existing stack of variables so that they are
            -- realtive to ctx'
            vs' :: Set Var
            vs' = Set.mapMonotonic (over varIndex (\y -> y - consumed)) vs
        output (shiftVar offset v)
        goCapture
          ctx'
          (offset + consumed)
          -- shift only the body so that its indices are relatie to ctx'.
          -- the +1 is needed because we are under a new lambda.
          (mkLambdaB bi (shift (-consumed + 1) body'))
          (freevarsbi' <> vs')

lambdaLiftBinder :: Member InfoTableBuilder r => BinderList Binder -> Binder -> Sem r Binder
lambdaLiftBinder bl = traverseOf binderType (lambdaLiftNode bl)

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
        goLambda :: Lambda -> Sem r Recur
        goLambda lm = do
          bi' <- lambdaLiftBinder bl (lm ^. lambdaBinder)
          l' <- lambdaLiftNode (BL.cons bi' bl) (NLam lm)
          let (freevarsAssocs, fBody') = captureFreeVars2 bl l'
              allfreevars :: [Var]
              allfreevars = map fst freevarsAssocs
              argsInfo :: [ArgumentInfo]
              argsInfo = map (argumentInfoFromBinder . snd) freevarsAssocs
          f <- freshSymbol
          registerIdent
            IdentifierInfo
              { _identifierSymbol = f,
                _identifierName = Nothing,
                _identifierType = typeFromArgs argsInfo,
                _identifierArgsNum = length allfreevars,
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
          letRecBinders' :: [Binder] <- mapM (lambdaLiftBinder bl) (letr ^.. letRecValues . each . letItemBinder)
          let bl' :: BinderList Binder
              -- the reverse is necessary because the last item in letRecBinders has index 0
              bl' = BL.prependRev (reverse letRecBinders') bl
          topSyms :: [Symbol] <- forM defs (const freshSymbol)

          let recItemsFreeVars :: [(Var, Binder)]
              recItemsFreeVars = mapMaybe helper (concatMap (freeVarsAll' bl') defs)
                where
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
                    | (sym, (itemBinder, b)) <- zipExact topSyms (zipExact letRecBinders' liftedDefs)
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
