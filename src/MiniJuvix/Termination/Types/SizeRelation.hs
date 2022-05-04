module MiniJuvix.Termination.Types.SizeRelation where

import Data.Semiring
import MiniJuvix.Prelude
import Prettyprinter

data Rel
  = RJust Rel'
  | RNothing
  deriving stock (Eq, Show, Generic)

data Rel'
  = REq
  | RLe
  deriving stock (Eq, Show, Generic)

instance Hashable Rel'

instance Hashable Rel

toRel :: Rel' -> Rel
toRel = RJust

add :: Rel -> Rel -> Rel
add RNothing b = b
add a RNothing = a
add (RJust a) (RJust b) = RJust (add' a b)

add' :: Rel' -> Rel' -> Rel'
add' RLe _ = RLe
add' REq b = b

mul :: Rel -> Rel -> Rel
mul RNothing _ = RNothing
mul _ RNothing = RNothing
mul (RJust a) (RJust b) = RJust (mul' a b)

mul' :: Rel' -> Rel' -> Rel'
mul' REq a = a
mul' RLe _ = RLe

star_ :: Rel -> Rel
star_ RNothing = RJust REq
star_ (RJust RLe) = RJust RLe
star_ (RJust REq) = RJust REq

instance Semigroup Rel where
  (<>) = add

instance Semiring Rel where
  one = RJust REq
  zero = RNothing
  plus = add
  times = mul

instance Pretty Rel where
  pretty r = case r of
    RJust r' -> pretty r'
    RNothing -> pretty ("?" :: Text)

instance Pretty Rel' where
  pretty r = case r of
    REq -> pretty ("=" :: Text)
    RLe -> pretty ("≺" :: Text)
