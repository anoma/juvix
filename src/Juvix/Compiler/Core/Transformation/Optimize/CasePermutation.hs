module Juvix.Compiler.Core.Transformation.Optimize.CasePermutation (casePermutation) where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Transformation.Base

isConstructorTree :: Case -> Node -> Bool
isConstructorTree c node = case run $ runFail $ go mempty node of
  Just ctrsMap ->
    all (checkOne ctrsMap) tags && checkDefault ctrsMap (c ^. caseDefault)
  Nothing -> False
  where
    tags = map (^. caseBranchTag) (c ^. caseBranches)

    checkOne :: HashMap Tag Int -> Tag -> Bool
    checkOne ctrsMap tag = case HashMap.lookup tag ctrsMap of
      Just 1 -> True
      Nothing -> True
      _ -> {- isImmediate -} False

    checkDefault :: HashMap Tag Int -> Maybe Node -> Bool
    checkDefault ctrsMap = \case
      Just {} ->
        -- or isImmediate
        sum (HashMap.filterWithKey (\k _ -> not (HashSet.member k tags')) ctrsMap) <= 1
        where
          tags' = HashSet.fromList tags
      Nothing -> True

    go :: (Member Fail r) => HashMap Tag Int -> Node -> Sem r (HashMap Tag Int)
    go ctrs = \case
      NCtr Constr {..} -> return $ HashMap.alter (Just . maybe 1 (+ 1)) _constrTag ctrs
      NCase Case {..} -> foldM go ctrs (map (^. caseBranchBody) _caseBranches)
      _ -> fail

convertNode :: Node -> Node
convertNode = dmap go
  where
    go :: Node -> Node
    go node = case node of
      NCase c@Case {..} -> case _caseValue of
        NCase c'
          | isConstructorTree c _caseValue ->
              NCase
                c'
                  { _caseBranches = map permuteBranch (c' ^. caseBranches),
                    _caseDefault = fmap (mkBody c) (c' ^. caseDefault)
                  }
          where
            permuteBranch :: CaseBranch -> CaseBranch
            permuteBranch br@CaseBranch {..} =
              case shift _caseBranchBindersNum (NCase c {_caseValue = mkBottom'}) of
                NCase cs ->
                  over caseBranchBody (mkBody cs) br
                _ -> impossible

            mkBody :: Case -> Node -> Node
            mkBody cs n = NCase cs {_caseValue = n}
        _ ->
          node
      _ -> node

casePermutation :: InfoTable -> InfoTable
casePermutation = mapAllNodes convertNode
