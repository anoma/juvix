module Juvix.Compiler.Core.Translation.FromInternal.Data.IndexTable where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Language
import Juvix.Compiler.Internal.Data.Name

data IndexTable = IndexTable
  { _indexTableVarsNum :: Index,
    _indexTableVars :: HashMap NameId Index
  }

makeLenses ''IndexTable

initIndexTable :: IndexTable
initIndexTable = IndexTable 0 mempty

localAddName :: (Member (Reader IndexTable) r) => Name -> Sem r a -> Sem r a
localAddName n = localAddNames [n]

localAddNamesHelper :: [Name] -> IndexTable -> IndexTable
localAddNamesHelper names tbl =
  let idx = tbl ^. indexTableVarsNum
      newElems = zip (map (^. nameId) names) [idx ..]
   in ( over indexTableVars (insertMany newElems)
          . over indexTableVarsNum (+ len)
      )
        tbl
  where
    len :: Int = length names

    insertMany :: [(NameId, Index)] -> HashMap NameId Index -> HashMap NameId Index
    insertMany l t = foldl' (\m (k, v) -> HashMap.insert k v m) t l

localAddNameSt ::
  forall r.
  (Member (State IndexTable) r) =>
  Name ->
  Sem r ()
localAddNameSt = localAddNamesSt . pure

localAddNamesSt ::
  forall r.
  (Member (State IndexTable) r) =>
  [Name] ->
  Sem r ()
localAddNamesSt names = modify (localAddNamesHelper names)

localAddNames ::
  forall r a.
  (Member (Reader IndexTable) r) =>
  [Name] ->
  Sem r a ->
  Sem r a
localAddNames names s = local (localAddNamesHelper names) s

underBinder :: (Members '[Reader IndexTable] r) => Sem r a -> Sem r a
underBinder = underBinders 1

underBinders :: (Members '[Reader IndexTable] r) => Int -> Sem r a -> Sem r a
underBinders nBinders = local (over indexTableVarsNum (+ nBinders))
