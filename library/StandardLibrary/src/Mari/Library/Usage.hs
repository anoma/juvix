{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Each binder and local context element in Mari is annotated with
-- a /usage/, which tracks how many times it is needed in a
-- runtime-relevant way.
module Mari.Library.Usage
  ( -- Usage type
    Usage (..),
    T,
    -- Utils
    allows,
    isNotZero,
    isZero,
    minus,
    pred,
    predPosUsage,
    toUsage,
  )
where

------------------------------------------------------------------------------

import qualified Data.Aeson as A
import Data.Maybe (fromJust)
import Mari.Library hiding (pred, show)
import qualified Mari.Library.PrettyPrint as PP

------------------------------------------------------------------------------

-- A usage is either a natural number specifying an exact usage (i.e.,
-- not an upper bound), or "any", meaning that usage is not tracked
-- for that variable and any number of usages is allowed.
data Usage = SNat Natural | SAny
  deriving (Eq, Show, Read, Generic, Data, NFData)

type T = Usage

------------------------------------------------------------------------------
-- The type of usages forms an ordered semiring.
------------------------------------------------------------------------------

instance Monoid Usage where
  mempty = SNat 0

instance Semigroup Usage where
  SNat π <> SNat ρ = SNat (π + ρ)
  SAny <> _ = SAny
  _ <> SAny = SAny

instance Semiring Usage where
  one = SNat 1

  SNat 0 <.> _ = SNat 0
  _ <.> SNat 0 = SNat 0
  SNat π <.> SNat ρ = SNat (π * ρ)
  SAny <.> _ = SAny
  _ <.> SAny = SAny

instance Ord Usage where
  compare (SNat a) (SNat b) = compare a b
  compare (SNat _) SAny = LT
  compare SAny (SNat _) = GT
  compare SAny SAny = EQ

------------------------------------------------------------------------------

type instance PP.Ann Usage = ()

instance PP.PrettySyntax Usage where
  pretty' (SNat π) = pure . PP.show $ π
  pretty' SAny = pure "ω"

instance A.ToJSON Usage where
  toJSON =
    A.genericToJSON
      ( A.defaultOptions
          { A.sumEncoding = A.ObjectWithSingleField
          }
      )

instance A.FromJSON Usage where
  parseJSON =
    A.genericParseJSON
      ( A.defaultOptions
          { A.sumEncoding = A.ObjectWithSingleField
          }
      )

------------------------------------------------------------------------------
-- Utils
------------------------------------------------------------------------------

isZero :: Usage -> Bool
isZero (SNat 0) = True
isZero _ = False

isNotZero :: Usage -> Bool
isNotZero = not . isZero

toUsage :: Integer -> Usage
toUsage = SNat . fromInteger

infixl 6 `minus`

minus :: Usage -> Usage -> Maybe Usage
minus SAny _ = Just SAny
minus (SNat π) (SNat ρ)
  | π >= ρ = Just . SNat $ π - ρ
minus _ _ = Nothing

pred :: Usage -> Maybe Usage
pred π = π `minus` SNat 1

predPosUsage :: Usage -> Usage
predPosUsage = fromJust . pred

-- | Usage compatibility.
allows :: Usage -> Usage -> Bool
allows (SNat x) (SNat y) = x == y
allows (SNat _) SAny = False
allows SAny (SNat _) = True
allows SAny SAny = True

infix 4 `allows`
