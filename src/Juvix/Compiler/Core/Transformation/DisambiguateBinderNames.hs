module Juvix.Compiler.Core.Transformation.DisambiguateBinderNames where

import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Juvix.Compiler.Core.Data.BinderList qualified as BL
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info.NameInfo (setInfoName)
import Juvix.Compiler.Core.Transformation.Base
import Text.Read qualified as Text

disambiguateNodeBinderNames :: InfoTable -> Node -> Node
disambiguateNodeBinderNames tab = dmapL go
  where
    go :: BinderList Binder -> Node -> Node
    go bl node = case node of
      NVar Var {..} ->
        mkVar (setInfoName (BL.lookup _varIndex bl ^. binderName) _varInfo) _varIndex
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
        | pi ^. piBinder . binderName /= "?" ->
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
              || HashMap.member name (tab ^. identMap) ->
              disambiguate bl (prime name)
          | otherwise ->
              name

    prime :: Text -> Text
    prime "" = "_X"
    prime "?" = "_X"
    prime name = case Text.splitOn "'" name of
      [name', ""] -> name' <> "'0"
      [name', num] -> name' <> "'" <> maybe (num <> "'") (show . (+ 1)) (Text.readMaybe (unpack num) :: Maybe Word)
      _ -> name <> "'"

disambiguateBinderNames :: InfoTable -> InfoTable
disambiguateBinderNames tab =
  mapAllNodes (disambiguateNodeBinderNames tab) tab
