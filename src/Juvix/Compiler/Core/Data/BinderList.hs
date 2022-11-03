module Juvix.Compiler.Core.Data.BinderList where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Language.Base

data BinderList a = BinderList
  { _blLength :: Int,
    _blMap :: HashMap Index a
  }

makeLenses ''BinderList

fromList :: [a] -> BinderList a
fromList l = BinderList (length l) (HashMap.fromList (zip [0 ..] l))

instance Foldable BinderList where
  foldr :: (a -> b -> b) -> b -> BinderList a -> b
  foldr f acc bl = foldr f acc (toList bl)

  null :: BinderList a -> Bool
  null = (== 0) . (^. blLength)

  length :: BinderList a -> Int
  length = (^. blLength)

  toList :: BinderList a -> [a]
  toList bl =
    map snd $
      sortBy (compare `on` fst) $
        HashMap.toList (bl ^. blMap)

lookup' :: Index -> BinderList a -> a
lookup' idx bl =
  fromMaybe err (HashMap.lookup target (bl ^. blMap))
  where
    target = idx
    err :: a
    err =
      error
        ( "invalid binder lookup. Got index "
            <> show idx
            <> " that targets "
            <> show target
            <> " and the length is "
            <> show (bl ^. blLength)
        )

lookup :: Index -> BinderList a -> a
lookup idx bl =
  fromMaybe err (HashMap.lookup target (bl ^. blMap))
  where
    target = bl ^. blLength - 1 - idx
    err :: a
    err =
      error
        ( "invalid binder lookup. Got index "
            <> show idx
            <> " that targets "
            <> show target
            <> " and the length is "
            <> show (bl ^. blLength)
        )

instance Semigroup (BinderList a) where
  a <> b = prepend (toList a) b

instance Monoid (BinderList a) where
  mempty =
    BinderList
      { _blLength = 0,
        _blMap = mempty
      }

extend :: a -> BinderList a -> BinderList a
extend a bl =
  BinderList
    (bl ^. blLength + 1)
    (HashMap.insert (bl ^. blLength) a (bl ^. blMap))

instance Functor BinderList where
  fmap :: (a -> b) -> BinderList a -> BinderList b
  fmap f = over blMap (fmap f)

prepend :: [a] -> BinderList a -> BinderList a
prepend l bl = foldr extend bl l
