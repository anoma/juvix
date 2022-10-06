module Juvix.Compiler.Core.Transformation.LambdaLifting
  ( module Juvix.Compiler.Core.Transformation.LambdaLifting,
    module Juvix.Compiler.Core.Transformation.Base,
  )
where

import Juvix.Compiler.Core.Data.BinderList (BinderList)
import Juvix.Compiler.Core.Data.BinderList qualified as BL
import Juvix.Compiler.Core.Data.InfoTableBuilder
import Juvix.Compiler.Core.Extra
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
        l' <- lambdaLiftNode bl (NLam lm)
        let freevars = toList (getFreeVars l')
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
        let defs :: NonEmpty Node
            defs = letr ^. letRecValues
        topSymsAssocs :: NonEmpty (Symbol, Node) <- forM defs $ \d -> do
                                               s' <- freshSymbol
                                               return (s', d)
        let topSyms :: NonEmpty Symbol = fst <$> topSymsAssocs
            freevars = toList (getFreeVars (NRec letr))
            freevarsAssocs :: [(Index, Info)]
            freevarsAssocs = [(i, BL.lookup i bl) | i <- map (^. varIndex) freevars]
            topCall :: Symbol -> Node
            topCall s = mkApps' (mkIdent' s) (map NVar freevars)
            topBody :: Node -> Sem r Node
            topBody b =
                captureFreeVars freevarsAssocs . substs (map topCall (toList topSyms))
                <$> lambdaLiftNode bl b
            letDef :: Symbol -> Node
            letDef s = mkApps' (mkIdent' s) (map NVar freevars)
        body' <- lambdaLiftNode bl (letr ^. letRecBody)
        forM_ topSymsAssocs $ \(s, a) -> topBody a >>= registerIdentNode s
        let letdefs' :: NonEmpty Node
            letdefs' = letDef <$> topSyms
            -- free variables in the lets and the body need to be shifted
            -- because we are introducing binders.
            -- the topmost let is shifted 0
            -- the lowermost is shifted (len - 1)
            -- the final body is shifted len
            shiftHelper :: Node -> NonEmpty Node -> Node
            shiftHelper b = goShift 0
              where
              goShift :: Int -> NonEmpty Node -> Node
              goShift k = \case
                x :| yys -> case yys of
                  [] -> shift k (mkLet' x b)
                  (y : ys) -> mkLet' (shift k x) (goShift (k + 1) (y :| ys))
        let res :: Node
            res = shiftHelper body' letdefs'
        return (End res)


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
