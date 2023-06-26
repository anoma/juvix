module Juvix.Compiler.Core.Transformation.LambdaLetRecLifting
  ( module Juvix.Compiler.Core.Transformation.LambdaLetRecLifting,
    module Juvix.Compiler.Core.Transformation.Base,
  )
where

import Juvix.Compiler.Core.Data.BinderList qualified as BL
import Juvix.Compiler.Core.Data.InfoTableBuilder
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info.NameInfo
import Juvix.Compiler.Core.Info.PragmaInfo
import Juvix.Compiler.Core.Pretty
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Transformation.ComputeTypeInfo (computeNodeType)

lambdaLiftBinder :: Members '[Reader OnlyLetRec, InfoTableBuilder] r => BinderList Binder -> Binder -> Sem r Binder
lambdaLiftBinder bl = traverseOf binderType (lambdaLiftNode bl)

type OnlyLetRec = Bool

lambdaLiftNode :: forall r. Members '[Reader OnlyLetRec, InfoTableBuilder] r => BinderList Binder -> Node -> Sem r Node
lambdaLiftNode aboveBl top =
  let topArgs :: [LambdaLhs]
      (topArgs, body) = unfoldLambdas top
   in goTop aboveBl body topArgs
  where
    nodeType :: Node -> Sem r Type
    nodeType n = flip computeNodeType n <$> getInfoTable

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
        goLambda l = do
          onlyLetRec <- ask @OnlyLetRec
          if
              | onlyLetRec -> return (Recur (NLam l))
              | otherwise -> goLambdaGo l
          where
            goLambdaGo :: Lambda -> Sem r Recur
            goLambdaGo lm = do
              l' <- lambdaLiftNode bl (NLam lm)
              let (freevarsAssocs, fBody') = captureFreeVarsCtx bl l'
                  allfreevars :: [Var]
                  allfreevars = map fst freevarsAssocs
                  argsNum :: Int
                  argsNum = length (fst (unfoldLambdas fBody'))
              f <- freshSymbol
              let name = uniqueName "lambda" f
              ty <- nodeType fBody'
              registerIdent
                name
                IdentifierInfo
                  { _identifierSymbol = f,
                    _identifierName = name,
                    _identifierLocation = Nothing,
                    _identifierType = ty,
                    _identifierArgsNum = argsNum,
                    _identifierIsExported = False,
                    _identifierBuiltin = Nothing,
                    _identifierPragmas = getInfoPragma (l ^. lambdaInfo),
                    _identifierArgNames = []
                  }
              registerIdentNode f fBody'
              let fApp = mkApps' (mkIdent (setInfoName name mempty) f) (map NVar allfreevars)
              return (End fApp)

        goLetRec :: LetRec -> Sem r Recur
        goLetRec letr = do
          let defs :: [Node]
              defs = letr ^.. letRecValues . each . letItemValue
              defsTypes :: [Type]
              defsTypes = letr ^.. letRecValues . each . letItemBinder . binderType
              ndefs :: Int
              ndefs = length defs
              binders :: [Binder]
              binders = letr ^.. letRecValues . each . letItemBinder
              pragmas :: [Pragmas]
              pragmas = getInfoPragmas (letr ^. letRecInfo)

          letRecBinders' :: [Binder] <- mapM (lambdaLiftBinder bl) binders
          topSyms :: [Symbol] <- forM defs (const freshSymbol)
          let bl' :: BinderList Binder
              bl' = BL.prependRev letRecBinders' bl

              topNames :: [Text]
              topNames = zipWithExact uniqueName (map (^. binderName) letRecBinders') topSyms

              topSymsWithName :: [(Symbol, Text)]
              topSymsWithName = zipExact topSyms topNames

              recItemsFreeVars :: [(Var, Binder)]
              recItemsFreeVars = mapMaybe helper (freeVarsCtxMany' bl' defs)
                where
                  -- discards free variables that refer to the letrec items
                  helper :: Var -> Maybe (Var, Binder)
                  helper v
                    | v ^. varIndex < ndefs = Nothing
                    | otherwise = Just (shiftVar (-ndefs) v, BL.lookup idx' bl)
                    where
                      idx' = v ^. varIndex - ndefs

              letItems :: [Node]
              letItems =
                [ mkApps' (mkIdent (setInfoName name mempty) sym) (map (NVar . fst) recItemsFreeVars)
                  | (sym, name) <- topSymsWithName
                ]

              subsCalls :: Node -> Node
              subsCalls = substs (reverse letItems)

          liftedDefs <- mapM (fmap subsCalls . lambdaLiftNode bl') defs
          body' <- lambdaLiftNode bl' (letr ^. letRecBody)
          let declareTopSyms :: Sem r ()
              declareTopSyms =
                sequence_
                  [ do
                      let (topBody, topTy) =
                            captureFreeVarsType
                              (map (first (^. varIndex)) recItemsFreeVars)
                              (b, bty)
                          argsNum :: Int
                          argsNum = length (fst (unfoldLambdas topBody))
                      registerIdentNode sym topBody
                      registerIdent
                        name
                        IdentifierInfo
                          { _identifierSymbol = sym,
                            _identifierName = name,
                            _identifierLocation = itemBinder ^. binderLocation,
                            _identifierType = topTy,
                            _identifierArgsNum = argsNum,
                            _identifierIsExported = False,
                            _identifierBuiltin = Nothing,
                            _identifierPragmas = pragma,
                            _identifierArgNames = []
                          }
                    | (((sym, name), (itemBinder, (b, bty))), pragma) <-
                        zip
                          ( zipExact
                              topSymsWithName
                              ( zipExact
                                  letRecBinders'
                                  (zipExact liftedDefs defsTypes)
                              )
                          )
                          (pragmas ++ repeat mempty)
                  ]
          declareTopSyms

          let -- TODO it can probably be simplified, and it's wrong
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
                        bnd' = over binderType (shift k) bnd
          let res :: Node
              res = shiftHelper body' (nonEmpty' (zipExact letItems letRecBinders'))
          return (Recur res)

lifting :: Bool -> InfoTable -> InfoTable
lifting onlyLetRec = run . runReader onlyLetRec . mapT' (const (lambdaLiftNode mempty))

lambdaLetRecLifting :: InfoTable -> InfoTable
lambdaLetRecLifting = lifting False

letRecLifting :: InfoTable -> InfoTable
letRecLifting = lifting True

nodeIsLifted :: Node -> Bool
nodeIsLifted = nodeIsLambdaLifted .&&. nodeIsLetRecLifted

-- | True if lambdas are only found at the top level
nodeIsLambdaLifted :: Node -> Bool
nodeIsLambdaLifted = not . hasNestedLambdas
  where
    hasNestedLambdas :: Node -> Bool
    hasNestedLambdas = has (cosmos . _NLam) . snd . unfoldLambdas'

-- | True if there are no letrec nodes
nodeIsLetRecLifted :: Node -> Bool
nodeIsLetRecLifted = not . hasLetRecs
  where
    hasLetRecs :: Node -> Bool
    hasLetRecs = has (cosmos . _NRec)

isLifted :: InfoTable -> Bool
isLifted = all nodeIsLifted . (^. identContext)

isLetRecLifted :: InfoTable -> Bool
isLetRecLifted = all nodeIsLetRecLifted . (^. identContext)
