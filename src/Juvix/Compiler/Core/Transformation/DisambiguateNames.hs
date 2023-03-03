module Juvix.Compiler.Core.Transformation.DisambiguateNames where

import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Compiler.Core.Data.BinderList qualified as BL
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info.NameInfo (setInfoName)
import Juvix.Compiler.Core.Transformation.Base

disambiguateNodeNames :: InfoTable -> Node -> Node
disambiguateNodeNames tab = dmapL go
  where
    go :: BinderList Binder -> Node -> Node
    go bl node = case node of
      NVar Var {..} ->
        mkVar (setInfoName (BL.lookup _varIndex bl ^. binderName) _varInfo) _varIndex
      NIdt Ident {..} ->
        mkIdent (setInfoName (identName tab _identSymbol) _identInfo) _identSymbol
      NLam lam ->
        NLam (over lambdaBinder (over binderName (disambiguate bl)) lam)
      NLet lt ->
        NLet (over letItem (over letItemBinder (over binderName (disambiguate bl))) lt)
      NRec lt ->
        NRec
          ( set
              letRecValues
              ( NonEmpty.fromList $
                  zipWithExact
                    (set letItemBinder)
                    (disambiguateBinders bl (map (^. letItemBinder) vs))
                    vs
              )
              lt
          )
        where
          vs = toList (lt ^. letRecValues)
      NCase c ->
        NCase (over caseBranches (map (over caseBranchBinders (disambiguateBinders bl))) c)
      NMatch m ->
        NMatch (over matchBranches (map (over matchBranchPatterns (NonEmpty.fromList . snd . disambiguatePatterns bl . toList))) m)
      NPi pi
        | varOccurs 0 (pi ^. piBody) ->
            NPi (over piBinder (over binderName (disambiguate bl)) pi)
      _ -> node

    disambiguateBinders :: BinderList Binder -> [Binder] -> [Binder]
    disambiguateBinders bl = \case
      [] -> []
      b : bs' -> b' : disambiguateBinders (BL.cons b' bl) bs'
        where
          b' = over binderName (disambiguate bl) b

    disambiguatePatterns :: BinderList Binder -> [Pattern] -> (BinderList Binder, [Pattern])
    disambiguatePatterns bl = \case
      [] -> (bl, [])
      pat : pats' -> case pat of
        PatWildcard {} -> second (pat :) (disambiguatePatterns bl pats')
        PatBinder pb -> second (PatBinder pb' :) (disambiguatePatterns bl' pats')
          where
            b' = over binderName (disambiguate bl) (pb ^. patternBinder)
            (bl', p') = disambiguatePatterns (BL.cons b' bl) [pb ^. patternBinderPattern]
            pb' = set patternBinderPattern (List.head p') (set patternBinder b' pb)
        PatConstr c -> second (pat' :) (disambiguatePatterns bl' pats')
          where
            (bl', args') = disambiguatePatterns bl (c ^. patternConstrArgs)
            pat' = PatConstr $ set patternConstrArgs args' c

    disambiguate :: BinderList Binder -> Text -> Text
    disambiguate bl name =
      if
          | name == "_" ->
              name
          | elem name (map (^. binderName) (toList bl))
              || HashSet.member name names ->
              disambiguate bl (prime name)
          | otherwise ->
              name

    names :: HashSet Text
    names = identNames tab

disambiguateNames :: InfoTable -> InfoTable
disambiguateNames tab =
  mapAllNodes (disambiguateNodeNames tab) tab
