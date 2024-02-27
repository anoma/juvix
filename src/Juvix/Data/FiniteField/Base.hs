module Juvix.Data.FiniteField.Base where

import Juvix.Prelude.Base

class (Fractional k) => FiniteField k where
  -- | The order is number of elements of a finite field @k@.
  -- It of the form @p^n@, where @p@ is  prime number called the characteristic
  -- of the field, and @n@ is a positive integer.
  order :: k -> Integer

  -- | The characteristic of a field, @p@.
  char :: k -> Integer

  -- | The inverse of Frobenius endomorphism @x@ ↦ @x^p@.
  pthRoot :: k -> k

  -- | All values of a field
  allValues :: [k]
