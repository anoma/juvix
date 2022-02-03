module MiniJuvix.Syntax.Concrete.Fixity where

import Language.Haskell.TH.Syntax (Lift)
import MiniJuvix.Utils.Prelude

data Precedence =
  PrecMinusOmega
  | PrecNat Natural
  | PrecOmega
  deriving stock (Show, Eq, Lift)

instance Ord Precedence where
  compare a b = case (a, b) of
    (PrecMinusOmega, PrecMinusOmega) -> EQ
    (PrecMinusOmega, _) -> LT
    (PrecNat _, PrecMinusOmega) -> GT
    (PrecNat n, PrecNat m) -> compare n m
    (PrecNat _, PrecOmega) -> LT
    (PrecOmega, PrecOmega) -> EQ
    (PrecOmega, _) -> GT


data UnaryAssoc = AssocPrefix | AssocPostfix
  deriving stock (Show, Eq, Ord, Lift)

data BinaryAssoc = AssocNone | AssocLeft | AssocRight
  deriving stock (Show, Eq, Ord, Lift)

data OperatorArity
  = Unary
      { unaryAssoc :: UnaryAssoc
      }
  | Binary
      { binaryAssoc :: BinaryAssoc
      }
  deriving stock (Show, Eq, Ord, Lift)

data Fixity = Fixity
  { fixityPrecedence :: Precedence,
    fixityArity :: OperatorArity
  }
  deriving stock (Show, Eq, Ord, Lift)
