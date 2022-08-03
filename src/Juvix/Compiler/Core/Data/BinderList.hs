module Juvix.Compiler.Core.Data.BinderList where

import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Juvix.Compiler.Core.Language.Base

data BinderList a = BinderList
  { _blLength :: Int,
    _blMap :: HashMap Index a
  }

makeLenses ''BinderList

fromList :: [a] -> BinderList a
fromList l = BinderList (List.length l) (HashMap.fromList (zip [0 ..] l))

toList :: BinderList a -> [a]
toList bl =
  List.map snd $
    sortBy (\x y -> compare (fst y) (fst x)) $
      HashMap.toList (bl ^. blMap)

empty :: BinderList a
empty = BinderList 0 mempty

length :: BinderList a -> Int
length = (^. blLength)

lookup :: Index -> BinderList a -> a
lookup idx bl =
  fromMaybe
    (error "invalid binder lookup")
    (HashMap.lookup (bl ^. blLength - 1 - idx) (bl ^. blMap))

extend :: a -> BinderList a -> BinderList a
extend a bl =
  BinderList
    (bl ^. blLength + 1)
    (HashMap.insert (bl ^. blLength) a (bl ^. blMap))

map :: (a -> b) -> BinderList a -> BinderList b
map f bl = bl {_blMap = HashMap.map f (bl ^. blMap)}

prepend :: [a] -> BinderList a -> BinderList a
prepend l bl = foldr extend bl l
