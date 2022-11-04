module Juvix.Compiler.Core.Data.BinderList where

import Juvix.Compiler.Core.Language.Base hiding (uncons)
import Juvix.Prelude qualified as Prelude

-- | if we have \x\y. b, the binderlist in b is [y, x]
data BinderList a = BinderList
  { _blLength :: Int,
    _blMap :: [a]
  }

makeLenses ''BinderList

fromList :: [a] -> BinderList a
fromList l = BinderList (length l) l

drop' :: Int -> BinderList a -> BinderList a
drop' k (BinderList n l) = BinderList (n - k) (dropExact k l)

tail' :: BinderList a -> BinderList a
tail' = snd . fromJust . uncons

uncons :: BinderList a -> Maybe (a, BinderList a)
uncons l = second helper <$> Prelude.uncons (l ^. blMap)
  where
    helper m =
      BinderList
        { _blLength = l ^. blLength - 1,
          _blMap = m
        }

toIndexedList :: BinderList a -> [(Index, a)]
toIndexedList = zip [0 ..] . toList

instance Foldable BinderList where
  foldr :: (a -> b -> b) -> b -> BinderList a -> b
  foldr f acc bl = foldr f acc (toList bl)

  null :: BinderList a -> Bool
  null = (== 0) . (^. blLength)

  length :: BinderList a -> Int
  length = (^. blLength)

  toList :: BinderList a -> [a]
  toList = (^. blMap)

lookup' :: Index -> BinderList a -> a
lookup' idx bl
  | target < bl ^. blLength = (bl ^. blMap) !! target
  | otherwise = err
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
lookup idx bl
  | target < bl ^. blLength = (bl ^. blMap) !! target
  | otherwise = err
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
    (snoc (bl ^. blMap) a)

instance Functor BinderList where
  fmap :: (a -> b) -> BinderList a -> BinderList b
  fmap f = over blMap (fmap f)

prepend :: [a] -> BinderList a -> BinderList a
prepend l bl = foldr extend bl l
