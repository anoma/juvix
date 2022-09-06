module Juvix.Compiler.Asm.Data.Stack where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Prelude

data Stack a = Stack
  { _stackValues :: HashMap Int a,
    _stackHeight :: Int
  }

makeLenses ''Stack

empty :: Stack a
empty = Stack {_stackValues = mempty, _stackHeight = 0}

height :: Stack a -> Int
height = (^. stackHeight)

null :: Stack a -> Bool
null s = height s == 0

push :: a -> Stack a -> Stack a
push a s =
  Stack
    { _stackValues = HashMap.insert (s ^. stackHeight) a (s ^. stackValues),
      _stackHeight = s ^. stackHeight + 1
    }

pop :: Stack a -> Stack a
pop s =
  if
      | height s == 0 -> error "popping an empty stack"
      | otherwise ->
          Stack
            { _stackValues = HashMap.delete (s ^. stackHeight - 1) (s ^. stackValues),
              _stackHeight = s ^. stackHeight - 1
            }

top :: Stack a -> a
top s =
  fromMaybe (error "accessing an empty stack") $
    HashMap.lookup (s ^. stackHeight - 1) (s ^. stackValues)

-- | Read nth value from the bottom of the stack.
nth :: Int -> Stack a -> a
nth idx s =
  fromMaybe (error "invalid stack index") $
    HashMap.lookup idx (s ^. stackValues)
