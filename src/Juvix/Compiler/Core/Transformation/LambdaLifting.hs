module Juvix.Compiler.Core.Transformation.LambdaLifting
  ( module Juvix.Compiler.Core.Transformation.LambdaLifting,
    module Juvix.Compiler.Core.Transformation.Base,
  )
where

import Juvix.Compiler.Core.Data.BinderList (BinderList)
import Juvix.Compiler.Core.Data.BinderList qualified as BL
import Juvix.Compiler.Core.Data.InfoTableBuilder
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info.BinderInfo qualified as Info
import Juvix.Compiler.Core.Info.NameInfo
import Juvix.Compiler.Core.Pretty
import Juvix.Compiler.Core.Transformation.Base

lambdaLiftNode :: forall r. Member InfoTableBuilder r => BinderList Info -> Node -> Sem r Node
lambdaLiftNode aboveBl top =
  mkLambdas topArgs <$> dmapLRM' (topArgsBinderList <> aboveBl, go) body
  where
    (topArgs, body) = unfoldLambdas top
    topArgsBinderList :: BinderList Info
    topArgsBinderList = BL.fromList topArgs
    typeFromArgs :: [ArgumentInfo] -> Type
    typeFromArgs = \case
      [] -> mkDynamic' -- change this when we have type info about the body
      (a : as) -> mkPi' (a ^. argumentType) (typeFromArgs as)
    -- extracts the argument info from the binder
    go :: BinderList Info -> Node -> Sem r Recur
    go bl = \case
      NLam l -> goLambda l
      NRec l -> goLetRec l
      m -> return (Recur m)
      where
        goLambda :: Lambda -> Sem r Recur
        goLambda lm = do
          let lambdaBinder :: Info
              lambdaBinder = Info.getInfoBinder (lm ^. lambdaInfo)
          l' <- lambdaLiftNode (BL.extend lambdaBinder bl) (NLam lm)
          let freevars = toList (freeVarsSet l')
              freevarsAssocs :: [(Index, Info)]
              freevarsAssocs = [(i, BL.lookup i bl) | i <- map (^. varIndex) freevars]
              fBody' = captureFreeVars freevarsAssocs l'
              argsInfo :: [ArgumentInfo]
              argsInfo = map (argumentInfoFromInfo . snd) freevarsAssocs
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
          let fApp = mkApps' (mkIdent mempty f) (map NVar freevars)
          return (End fApp)

        goLetRec :: LetRec -> Sem r Recur
        goLetRec letr = do
          -- getInfoTable >>= traceM . ppTrace
          let defs :: [Node]
              defs = toList (letr ^. letRecValues)
              ndefs :: Int
              ndefs = length defs
              letRecBinders :: [Info]
              letRecBinders = Info.getInfoBinders ndefs (letr ^. letRecInfo)
              bl' :: BinderList Info
              bl' = BL.prepend letRecBinders bl
          topSyms :: [Symbol] <- forM defs (const freshSymbol)
          let recItemsFreeVars :: [(Var, Info)]
              recItemsFreeVars = mapMaybe helper (toList (mconcatMap freeVarsSet defs))
                where
                  -- free vars in each let
                  -- throw away variables bound in the letrec and shift others
                  helper :: Var -> Maybe (Var, Info)
                  helper v
                    | v ^. varIndex < ndefs = Nothing
                    | otherwise = Just (set varIndex idx' v, BL.lookup idx' bl)
                    where
                      idx' = (v ^. varIndex) - ndefs

              -- FIXME
              subsCalls :: Node -> Node
              subsCalls =
                substs
                  ( reverse
                      [ mkApps' (mkIdent' sym) (map (NVar . fst) recItemsFreeVars)
                        | sym <- topSyms
                      ]
                  )
          let fv = recItemsFreeVars
          -- traceM ("freevars " <> show (length fv) <> " " <> show (map (pretty . getInfoName . snd) fv))
          liftedDefs <- mapM (lambdaLiftNode bl . subsCalls) defs
          body' <- lambdaLiftNode bl' (letr ^. letRecBody)
          let -- free vars in each let
              -- recItemsFreeVars' :: [(Var, Info)]
              -- recItemsFreeVars' = mapMaybe helper (toList (mconcatMap freeVarsSet liftedDefs))
              --   where
              --     -- throw away variables bound in the letrec and shift others

              --     helper :: Var -> Maybe (Var, Info)
              --     helper v
              --       | idx < ndefs = Nothing
              --       | otherwise =
              --           let idx' = idx - ndefs
              --            in Just (v, BL.lookup idx' bl)
              --       where
              --         idx = v ^. varIndex
              -- -- replace calls to letrec items to a calls to the fresh top
              -- -- symbols.
              -- -- FIXME note that these calls can appear in the
              -- -- dynamically created top level nodes in recursive calls to
              -- -- lambda lifting
              -- subsCalls' :: Node -> Node
              -- subsCalls' =
              --   substs
              --     ( reverse
              --         [ mkApps' (mkIdent' sym) (map (NVar . fst) recItemsFreeVars)
              --           | sym <- topSyms
              --         ]
              --     )
              declareTopSyms :: Sem r ()
              declareTopSyms =
                sequence_
                  [ do
                      let fv = recItemsFreeVars
                          topBody = captureFreeVars (map (first (^. varIndex)) fv) b
                          argsInfo :: [ArgumentInfo]
                          argsInfo = map (argumentInfoFromInfo . snd) fv
                      registerIdentNode sym topBody
                      registerIdent
                        IdentifierInfo
                          { _identifierSymbol = sym,
                            _identifierName = getInfoName itemInfo,
                            _identifierType = typeFromArgs argsInfo,
                            _identifierArgsNum = length fv,
                            _identifierArgsInfo = argsInfo,
                            _identifierIsExported = False
                          }
                    | (sym, (itemInfo, b)) <- zipExact topSyms (zipExact letRecBinders liftedDefs)
                  ]
              letItems :: [Node]
              letItems =
                let fv = recItemsFreeVars
                 in [ mkApps' (mkIdent' s) (map (NVar . fst) fv)
                      | s <- topSyms
                    ]
          declareTopSyms

          -- getInfoTable >>= traceM . ppTrace
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
