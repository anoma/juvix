
module MiniJuvix.Syntax.Concrete.Fixity where

import MiniJuvix.Prelude

data Precedence =
  PrecMinusOmega
  | PrecNat Natural
  | PrecOmega
  deriving stock (Show, Eq, Data)

instance Ord Precedence where
  compare a b = case (a, b) of
    (PrecMinusOmega, PrecMinusOmega) -> EQ
    (PrecMinusOmega, _) -> LT
    (PrecNat _, PrecMinusOmega) -> GT
    (PrecNat n, PrecNat m) -> compare n m
    (PrecNat _, PrecOmega) -> LT
    (PrecOmega, PrecOmega) -> EQ
    (PrecOmega, _) -> GT


data UnaryAssoc = AssocPostfix
  deriving stock (Show, Eq, Ord, Data)

data BinaryAssoc = AssocNone | AssocLeft | AssocRight
  deriving stock (Show, Eq, Ord, Data)

data OperatorArity
  = Unary
      { unaryAssoc :: UnaryAssoc
      }
  | Binary
      { binaryAssoc :: BinaryAssoc
      }
  deriving stock (Show, Eq, Ord, Data)

data Fixity = Fixity
  { fixityPrecedence :: Precedence,
    fixityArity :: OperatorArity
  }
  deriving stock (Show, Eq, Ord, Data)
