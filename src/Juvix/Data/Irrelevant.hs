module Juvix.Data.Irrelevant where

import Juvix.Data.Loc
import Juvix.Extra.Serialize as S
import Juvix.Prelude.Base
import Juvix.Prelude.Pretty
import Prelude (show)

-- | Used when you annotate some AST with some information that you want to be
-- ignored when checking for equality/ordering
newtype Irrelevant a = Irrelevant
  { _unIrrelevant :: a
  }
  deriving stock (Generic, Data, Lift)

instance (Serialize a) => Serialize (Irrelevant a) where
  put (Irrelevant x) = S.put x
  get = Irrelevant <$> S.get

instance Show (Irrelevant a) where
  show = const "Irrelevant {}"

instance (HasLoc a) => HasLoc (Irrelevant a) where
  getLoc (Irrelevant a) = getLoc a

instance Eq (Irrelevant a) where
  _ == _ = True

instance Ord (Irrelevant a) where
  compare _ _ = EQ

instance (Pretty a) => Pretty (Irrelevant a) where
  pretty (Irrelevant a) = pretty a

instance Functor Irrelevant where
  fmap f (Irrelevant a) = Irrelevant (f a)

instance Applicative Irrelevant where
  pure :: a -> Irrelevant a
  pure = Irrelevant

  (<*>) :: Irrelevant (a -> b) -> Irrelevant a -> Irrelevant b
  Irrelevant f <*> Irrelevant a = Irrelevant (f a)

instance Monad Irrelevant where
  (Irrelevant mx) >>= f = f mx

makeLenses ''Irrelevant
