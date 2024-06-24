module Juvix.Compiler.Core.Transformation.Optimize.CasePermutation (casePermutation) where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Transformation.Base

-- | Checks if `node` is a case tree such that all leaves are constructor
-- applications, and each constructor `C` matched on in `c` either occurs as a
-- leaf in `node` at most once or the branch body in `c` associated with `C` is
-- an immediate value.
isConstructorTree :: Module -> Case -> Node -> Bool
isConstructorTree md c node = case run $ runFail $ go mempty node of
  Just ctrsMap ->
    all (checkOne ctrsMap) tags && checkDefault ctrsMap (c ^. caseDefault)
  Nothing -> False
  where
    tags = map (^. caseBranchTag) (c ^. caseBranches)
    tagMap = HashMap.fromList (map (\br -> (br ^. caseBranchTag, br ^. caseBranchBody)) (c ^. caseBranches))

    checkOne :: HashMap Tag Int -> Tag -> Bool
    checkOne ctrsMap tag = case HashMap.lookup tag ctrsMap of
      Just 1 -> True
      Nothing -> True
      _ -> isImmediate md (fromJust $ HashMap.lookup tag tagMap)

    checkDefault :: HashMap Tag Int -> Maybe Node -> Bool
    checkDefault ctrsMap = \case
      Just d ->
        sum (HashMap.filterWithKey (\k _ -> not (HashSet.member k tags')) ctrsMap) <= 1
          || isImmediate md d
        where
          tags' = HashSet.fromList tags
      Nothing -> True

    -- Returns the map from tags to their number of occurrences in the leaves of
    -- the case tree.
    go :: (Member Fail r) => HashMap Tag Int -> Node -> Sem r (HashMap Tag Int)
    go ctrs = \case
      NCtr Constr {..} ->
        return $ HashMap.alter (Just . maybe 1 (+ 1)) _constrTag ctrs
      NCase Case {..} -> do
        ctrs' <- maybe (return ctrs) (go ctrs) _caseDefault
        foldM go ctrs' (map (^. caseBranchBody) _caseBranches)
      _ ->
        fail

-- | Convert e.g. `case (if A C1 C2) of C1 := X | C2 := Y` to
-- `if A (case C1 of C1 := X | C2 := Y) (case C2 of C1 := X | C2 := Y)`
-- See: https://github.com/anoma/juvix/issues/2440
convertNode :: Module -> Node -> Node
convertNode md = dmap go
  where
    go :: Node -> Node
    go node = case node of
      NCase c@Case {..} -> case _caseValue of
        NCase c'
          | isConstructorTree md c _caseValue ->
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

casePermutation :: Module -> Module
casePermutation md = mapAllNodes (convertNode md) md
