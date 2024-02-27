-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Data.FiniteField.PrimeField
-- Copyright   :  (c) Masahiro Sakai 2013-2014
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable (ScopedTypeVariables, MultiParamTypeClasses, DeriveDataTypeable, TemplateHaskell, BangPatterns)
--
-- Finite field of prime order p, Fp = Z/pZ.
--
-- References:
--
-- * <http://en.wikipedia.org/wiki/Finite_field>
module Juvix.Data.FiniteField.PrimeField
  ( PrimeField,
    toInteger,

    -- * Template haskell utilities
    -- $TH
    primeField,
  )
where

import Data.Hashable
import Data.Ratio (denominator, numerator)
import Data.Singletons
import GHC.TypeLits
import GHC.TypeLits.Singletons ()
import Juvix.Data.FiniteField.Base
import Language.Haskell.TH qualified as TH
import Prelude hiding (toInteger)

-- | Finite field of prime order p, Fp = Z/pZ.
--
-- NB: Primality of @p@ is assumed, but not checked.
newtype PrimeField (p :: Natural)
  = PrimeField Integer
  deriving stock (Eq)

-- | conversion to 'Integer'
toInteger :: PrimeField p -> Integer
toInteger (PrimeField a) = a

toInt :: (Integral a) => PrimeField p -> a
toInt = fromInteger . toInteger

instance Show (PrimeField p) where
  showsPrec n (PrimeField x) = showsPrec n x

instance (SingI p) => Read (PrimeField p) where
  readsPrec n s = [(fromInteger a, s') | (a, s') <- readsPrec n s]

instance (SingI p) => Num (PrimeField p) where
  PrimeField a + PrimeField b = fromInteger $ a + b
  PrimeField a * PrimeField b = fromInteger $ a * b
  PrimeField a - PrimeField b = fromInteger $ a - b
  negate (PrimeField a) = fromInteger $ negate a
  abs a = a
  signum _ = 1
  fromInteger a = ret
    where
      ret = PrimeField $ a `mod` char ret

instance (SingI p) => Fractional (PrimeField p) where
  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

  --  recip a = a ^ (char a - 2 :: Integer)
  recip x@(PrimeField a) =
    case exgcd a p of
      (_, r, _) -> fromInteger r
    where
      p :: Integer
      p = char x

instance (SingI p) => Bounded (PrimeField p) where
  minBound = PrimeField 0
  maxBound = ret
    where
      ret :: PrimeField p
      ret = PrimeField (char ret - 1)

instance (SingI p) => Enum (PrimeField p) where
  toEnum x
    | toInteger (minBound :: PrimeField p) <= x' && x' <= toInteger (maxBound :: PrimeField p) = PrimeField x'
    | otherwise = error "PrimeField.toEnum: bad argument"
    where
      x' = fromIntegral x
  fromEnum = toInt

instance Ord (PrimeField p) where
  PrimeField a `compare` PrimeField b = a `compare` b

instance (SingI (p :: Natural)) => FiniteField (PrimeField p) where
  order x = char x

  -- char _    = natVal (Proxy :: Proxy p)
  char _ =
    let c :: Natural
        c = demote @p
     in fromIntegral c
  pthRoot a = a
  allValues = [minBound .. maxBound]

instance (SingI p) => Hashable (PrimeField p) where
  hashWithSalt s x@(PrimeField a) =
    s `hashWithSalt` char x `hashWithSalt` a

-- | Extended GCD algorithm
exgcd :: (Integral a) => a -> a -> (a, a, a)
exgcd f1 f2 = f $ go f1 f2 1 0 0 1
  where
    go :: forall t. (Integral t) => t -> t -> t -> t -> t -> t -> (t, t, t)
    go !r0 !r1 !s0 !s1 !t0 !t1
      | r1 == 0 = (r0, s0, t0)
      | otherwise = go r1 r2 s1 s2 t1 t2
      where
        (q, r2) = r0 `divMod` r1
        s2 = s0 - q * s1
        t2 = t0 - q * t1
    f :: forall a b c. (Ord a, Num a, Num b, Num c) => (a, b, c) -> (a, b, c)
    f (g, u, v)
      | g < 0 = (-g, -u, -v)
      | otherwise = (g, u, v)

-- ---------------------------------------------------------------------------

-- | Create a PrimeField type
primeField :: Integer -> TH.TypeQ
primeField n
  | n <= 0 = error "primeField: negative value"
  | otherwise = [t|PrimeField $(TH.litT (TH.numTyLit n))|]

-- $TH
-- Here is usage example for primeField:
--
-- > a :: $(primeField 15485867)
-- > a = 1
