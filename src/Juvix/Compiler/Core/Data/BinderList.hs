module Juvix.Compiler.Core.Data.BinderList where

import GHC.Show qualified as S
import Juvix.Compiler.Core.Language hiding (cons, drop, lookup, uncons)
import Juvix.Prelude qualified as Prelude

-- | if we have \x\y. b, the binderlist in b is [y, x]
data BinderList a = BinderList
  { _blLength :: Int,
    _blMap :: [a]
  }
  deriving stock (Eq, Generic)

makeLenses ''BinderList

fromList :: [a] -> BinderList a
fromList l = BinderList (length l) l

drop :: Int -> BinderList a -> BinderList a
drop k (BinderList n l) = BinderList (n - k) (dropExact k l)

tail :: BinderList a -> BinderList a
tail = snd . fromJust . uncons

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

instance Show a => Show (BinderList a) where
  show = S.show . toList

-- | same as `lookupsSortedRev` but the result is in the same order as the input list.
lookupsSorted :: BinderList a -> [Var' i] -> [(Var' i, a)]
lookupsSorted bl = reverse . lookupsSortedRev bl

-- | efficient multiple lookups. The input list needs to be in non-decreasing order.
-- | The result is in reversed order (non-increasing order)
lookupsSortedRev :: BinderList a -> [Var' i] -> [(Var' i, a)]
lookupsSortedRev bl = go [] 0 bl
  where
    go :: [(Var' i, a)] -> Index -> BinderList a -> [Var' i] -> [(Var' i, a)]
    go acc off ctx = \case
      [] -> acc
      (v : vs) ->
        let skipped = v ^. varIndex - off
            off' = off + skipped
            ctx' = drop skipped ctx
         in go ((v, head' ctx') : acc) off' ctx' vs
    head' :: BinderList a -> a
    head' = lookup 0

-- | lookup de Bruijn Index
lookup :: Index -> BinderList a -> a
lookup idx bl
  | idx < bl ^. blLength = (bl ^. blMap) !! idx
  | otherwise = err
  where
    err :: a
    err =
      error
        ( "invalid binder lookup. Got index "
            <> show idx
            <> " and the length is "
            <> show (bl ^. blLength)
            <> ". Actual length is "
            <> show (length (bl ^. blMap))
        )

-- | lookup de Bruijn Level
lookupLevel :: Level -> BinderList a -> a
lookupLevel lvl bl
  | target < bl ^. blLength = (bl ^. blMap) !! target
  | otherwise = err
  where
    target = bl ^. blLength - 1 - lvl
    err :: a
    err =
      error
        ( "invalid binder lookup. Got de Bruijn level "
            <> show lvl
            <> " that targets "
            <> show target
            <> " and the length is "
            <> show (bl ^. blLength)
        )

instance Semigroup (BinderList a) where
  (BinderList la ta) <> (BinderList lb tb) =
    BinderList
      { _blLength = la + lb,
        _blMap = ta <> tb
      }

instance Monoid (BinderList a) where
  mempty =
    BinderList
      { _blLength = 0,
        _blMap = mempty
      }

instance Functor BinderList where
  fmap :: (a -> b) -> BinderList a -> BinderList b
  fmap f = over blMap (fmap f)

cons :: a -> BinderList a -> BinderList a
cons a (BinderList l m) = BinderList (l + 1) (a : m)

-- | prepend [a,b] [c,d] = [a,b,c,d]
prepend :: [a] -> BinderList a -> BinderList a
prepend l bl = fromList l <> bl

-- | prependRev [a,b] [c,d] = [b,a,c,d]
-- more efficient than 'prepend' since it is tail recursive.
-- One example use case is prepending a list of binders in a letrec.
prependRev :: [a] -> BinderList a -> BinderList a
prependRev l (BinderList s m) =
  BinderList
    { _blLength = length l + s,
      _blMap = foldl' (flip (:)) m l
    }
