module Juvix.Compiler.Reg.Data.IndexMap where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Reg.Language.Base hiding (lookup)

data IndexMap k = IndexMap
  { _indexMapFirstFree :: Int,
    _indexMapTable :: HashMap k Index
  }

makeLenses ''IndexMap

instance (Hashable k) => Semigroup (IndexMap k) where
  m1 <> m2 =
    IndexMap
      { _indexMapTable = m1 ^. indexMapTable <> m2 ^. indexMapTable,
        _indexMapFirstFree = max (m1 ^. indexMapFirstFree) (m2 ^. indexMapFirstFree)
      }

instance (Hashable k) => Monoid (IndexMap k) where
  mempty =
    IndexMap
      { _indexMapFirstFree = 0,
        _indexMapTable = mempty
      }

assign :: (Hashable k) => IndexMap k -> k -> (Index, IndexMap k)
assign IndexMap {..} k =
  ( _indexMapFirstFree,
    IndexMap
      { _indexMapFirstFree = _indexMapFirstFree + 1,
        _indexMapTable = HashMap.insert k _indexMapFirstFree _indexMapTable
      }
  )

lookup :: (Hashable k) => IndexMap k -> k -> Index
lookup IndexMap {..} k = fromJust $ HashMap.lookup k _indexMapTable

combine :: forall k. (Hashable k) => IndexMap k -> IndexMap k -> IndexMap k
combine mp1 mp2 =
  IndexMap
    { _indexMapFirstFree = max (mp1 ^. indexMapFirstFree) (mp2 ^. indexMapFirstFree),
      _indexMapTable = mp
    }
  where
    mp =
      foldr
        (\k -> HashMap.update (checkVal k) k)
        (HashMap.intersection (mp1 ^. indexMapTable) (mp2 ^. indexMapTable))
        (HashMap.keys (mp2 ^. indexMapTable))

    checkVal :: k -> Index -> Maybe Index
    checkVal k idx
      | lookup mp2 k == idx = Just idx
      | otherwise = Nothing
