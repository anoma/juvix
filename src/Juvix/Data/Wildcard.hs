module Juvix.Data.Wildcard where

import Juvix.Data.Loc
import Juvix.Prelude.Base
import Prettyprinter

newtype Wildcard = Wildcard
  { _wildcardLoc :: Interval
  }
  deriving stock (Show)

makeLenses ''Wildcard

instance Eq Wildcard where
  _ == _ = True

instance Ord Wildcard where
  compare _ _ = EQ

instance Hashable Wildcard where
  hashWithSalt s _ = hashWithSalt s (0 :: Int)

instance HasLoc Wildcard where
  getLoc = (^. wildcardLoc)

instance Pretty Wildcard where
  pretty = const "_"
