module Juvix.Compiler.Reg.Data.IndexMap where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Reg.Language.Base

data IndexMap k = IndexMap
  { _indexMapSize :: Int,
    _indexMapTable :: HashMap k Index
  }

makeLenses ''IndexMap

instance (Hashable k) => Semigroup (IndexMap k) where
  m1 <> m2 =
    IndexMap
      { _indexMapTable = m,
        _indexMapSize = HashMap.size m
      }
    where
      m = m1 ^. indexMapTable <> m2 ^. indexMapTable

instance (Hashable k) => Monoid (IndexMap k) where
  mempty =
    IndexMap
      { _indexMapSize = 0,
        _indexMapTable = mempty
      }

assign :: (Hashable k) => IndexMap k -> k -> (Index, IndexMap k)
assign IndexMap {..} k =
  ( _indexMapSize,
    IndexMap
      { _indexMapSize = _indexMapSize + 1,
        _indexMapTable = HashMap.insert k _indexMapSize _indexMapTable
      }
  )

lookup :: (Hashable k) => IndexMap k -> k -> Index
lookup IndexMap {..} k = fromJust $ HashMap.lookup k _indexMapTable
