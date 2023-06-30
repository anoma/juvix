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

localAddName :: Member (Reader IndexTable) r => Name -> Sem r a -> Sem r a
localAddName n = localAddNames [n]

localAddNames :: forall r a. (Member (Reader IndexTable) r) => [Name] -> Sem r a -> Sem r a
localAddNames names s = do
  updateFn <- update
  local updateFn s
  where
    len :: Int = length names
    insertMany :: [(NameId, Index)] -> HashMap NameId Index -> HashMap NameId Index
    insertMany l t = foldl' (\m (k, v) -> HashMap.insert k v m) t l
    update :: Sem r (IndexTable -> IndexTable)
    update = do
      idx <- asks (^. indexTableVarsNum)
      let newElems = zip (map (^. nameId) names) [idx ..]
      return
        ( over indexTableVars (insertMany newElems)
            . over indexTableVarsNum (+ len)
        )

underBinders :: Members '[Reader IndexTable] r => Int -> Sem r a -> Sem r a
underBinders nBinders = local (over indexTableVarsNum (+ nBinders))
