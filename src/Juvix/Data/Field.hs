module Juvix.Data.Field where

import Data.Singletons.Decide
import GHC.TypeLits.Singletons ()
import Juvix.Data.FiniteField.PrimeField
import Juvix.Prelude hiding (toInteger)

newtype FField = FField
  { _unFField :: Sigma Natural (TyCon1 PrimeField)
  }

makeLenses ''FField

fieldAdd :: FField -> FField -> FField
fieldAdd
  (FField ((n1 :: Sing (p :: Natural)) :&: (f1 :: PrimeField p)))
  (FField ((n2 :: Sing (r :: Natural)) :&: (f2 :: PrimeField r))) =
    let x = decideEquality n1 n2
     in case x :: Maybe (p :~: r) of
          Just Refl -> FField (n1 :&: withSingI n1 (f1 + f2))
          Nothing -> impossible

fieldSub :: FField -> FField -> FField
fieldSub
  (FField ((n1 :: Sing (p :: Natural)) :&: (f1 :: PrimeField p)))
  (FField ((n2 :: Sing (r :: Natural)) :&: (f2 :: PrimeField r))) =
    let x = decideEquality n1 n2
     in case x :: Maybe (p :~: r) of
          Just Refl -> FField (n1 :&: withSingI n1 (f1 - f2))
          Nothing -> impossible

fieldMul :: FField -> FField -> FField
fieldMul
  (FField ((n1 :: Sing (p :: Natural)) :&: (f1 :: PrimeField p)))
  (FField ((n2 :: Sing (r :: Natural)) :&: (f2 :: PrimeField r))) =
    let x = decideEquality n1 n2
     in case x :: Maybe (p :~: r) of
          Just Refl -> FField (n1 :&: withSingI n1 (f1 * f2))
          Nothing -> impossible

fieldDiv :: FField -> FField -> FField
fieldDiv
  (FField ((n1 :: Sing (p :: Natural)) :&: (f1 :: PrimeField p)))
  (FField ((n2 :: Sing (r :: Natural)) :&: (f2 :: PrimeField r))) =
    let x = decideEquality n1 n2
     in case x :: Maybe (p :~: r) of
          Just Refl -> FField (n1 :&: withSingI n1 (f1 / f2))
          Nothing -> impossible

fieldFromInteger :: Natural -> Integer -> FField
fieldFromInteger fsize n =
  FField (withSomeSing fsize $ \s -> s :&: withSingI s (fromInteger n))

fieldSize :: FField -> Natural
fieldSize
  (FField ((n :: Sing (p :: Natural)) :&: (_ :: PrimeField p))) =
    fromSing n

fieldToInteger :: FField -> Integer
fieldToInteger
  (FField ((_ :: Sing (p :: Natural)) :&: (f1 :: PrimeField p))) =
    toInteger f1
