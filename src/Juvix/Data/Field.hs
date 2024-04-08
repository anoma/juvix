module Juvix.Data.Field where

import Data.Serialize as S
import Data.Singletons.Decide
import GHC.Show qualified as S
import GHC.TypeLits.Singletons ()
import Juvix.Data.FiniteField.PrimeField
import Juvix.Prelude hiding (toInteger)
import Juvix.Prelude.Pretty

newtype FField = FField
  { _unFField :: Sigma Natural (TyCon1 PrimeField)
  }
  deriving stock (Generic)

makeLenses ''FField

defaultFieldSize :: Natural
defaultFieldSize = cairoFieldSize

cairoFieldSize :: Natural
cairoFieldSize = 3618502788666131213697322783095070105623107215331596699973092056135872020481

smallFieldSize :: Natural
smallFieldSize = 2147483647

allowedFieldSizes :: [Natural]
allowedFieldSizes = [11, smallFieldSize, cairoFieldSize]

instance Serialize FField where
  put f = S.put (fieldSize f, fieldToInteger f)

  get = do
    (n, f) <- S.get
    return $ fieldFromInteger n f

instance Pretty FField where
  pretty f = pretty (fieldToInteger f)

instance Show FField where
  show f = show (fieldToInteger f)

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
  (FField ((n1 :: Sing (p :: Natural)) :&: (f1 :: PrimeField p)))
    | i < n `div` 2 = i
    | otherwise = -(n - i)
    where
      i = toInteger f1
      n = fromIntegral (fromSing n1)

fieldToNatural :: FField -> Natural
fieldToNatural f = (fromIntegral ((fieldToInteger f) + fromIntegral s)) `mod` s
  where
    s = fieldSize f

fieldResize :: Natural -> FField -> FField
fieldResize n f = fieldFromInteger n (fieldToInteger f)

fieldEq :: FField -> FField -> Bool
fieldEq
  (FField ((n1 :: Sing (p :: Natural)) :&: (f1 :: PrimeField p)))
  (FField ((n2 :: Sing (r :: Natural)) :&: (f2 :: PrimeField r))) =
    let x = decideEquality n1 n2
     in case x :: Maybe (p :~: r) of
          Just Refl -> f1 == f2
          Nothing -> False

instance Eq FField where
  (==) = fieldEq
