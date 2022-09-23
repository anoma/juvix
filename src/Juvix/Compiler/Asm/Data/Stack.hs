module Juvix.Compiler.Asm.Data.Stack where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Prelude hiding (empty)

data Stack a = Stack
  { _stackValues :: HashMap Int a,
    _stackHeight :: Int
  }

makeLenses ''Stack

empty :: Stack a
empty = Stack {_stackValues = mempty, _stackHeight = 0}

fromList :: [a] -> Stack a
fromList = foldr push empty

instance Foldable Stack where
  foldr :: (a -> b -> b) -> b -> Stack a -> b
  foldr f x = foldr f x . toList

  toList :: Stack a -> [a]
  toList =
    reverse
      . map snd
      . sortBy (\x y -> compare (fst x) (fst y))
      . HashMap.toList
      . (^. stackValues)

  length :: Stack a -> Int
  length = (^. stackHeight)

  null :: Stack a -> Bool
  null s = length s == 0

push :: a -> Stack a -> Stack a
push a s =
  Stack
    { _stackValues = HashMap.insert (s ^. stackHeight) a (s ^. stackValues),
      _stackHeight = s ^. stackHeight + 1
    }

pop :: Stack a -> Stack a
pop s =
  if
      | null s -> error "popping an empty stack"
      | otherwise ->
          Stack
            { _stackValues = HashMap.delete (s ^. stackHeight - 1) (s ^. stackValues),
              _stackHeight = s ^. stackHeight - 1
            }

top :: Stack a -> Maybe a
top s = HashMap.lookup (s ^. stackHeight - 1) (s ^. stackValues)

-- | Values on top of the stack, in order from top to bottom.
topValues :: Int -> Stack a -> Maybe [a]
topValues n s = mapM (`nthFromTop` s) [0 .. n - 1]

-- | Read nth value from the bottom of the stack.
nthFromBottom :: Int -> Stack a -> Maybe a
nthFromBottom idx s =
  HashMap.lookup idx (s ^. stackValues)

-- | Read nth value from the top of the stack.
nthFromTop :: Int -> Stack a -> Maybe a
nthFromTop idx s =
  HashMap.lookup (s ^. stackHeight - idx - 1) (s ^. stackValues)
