module MiniJuvix.Syntax.Fixity where

import Language.Haskell.TH.Syntax (Lift)
import MiniJuvix.Prelude

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

data UnaryAssoc = AssocPostfix
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

data Atomicity =
  Atom
  | Aggregate Fixity

class HasAtomicity a where
  atomicity :: a -> Atomicity

class HasFixity a where
  getFixity :: a -> Fixity

isLeftAssoc :: Fixity -> Bool
isLeftAssoc opInf = case fixityArity opInf of
    Binary AssocLeft -> True
    _ -> False

isRightAssoc :: Fixity -> Bool
isRightAssoc opInf = case fixityArity opInf of
    Binary AssocRight -> True
    _ -> False

isPostfixAssoc :: Fixity -> Bool
isPostfixAssoc opInf = case fixityArity opInf of
    Unary AssocPostfix -> True
    _ -> False

appFixity :: Fixity
appFixity = Fixity PrecOmega (Binary AssocLeft)

funFixity :: Fixity
funFixity = Fixity PrecMinusOmega (Binary AssocRight)

atomParens :: (Fixity -> Bool) -> Atomicity -> Fixity -> Bool
atomParens associates argAtom opInf = case argAtom of
  Atom -> False
  Aggregate argInf
   | argInf > opInf -> False
   | argInf < opInf -> True
   | associates opInf -> False
   | otherwise -> True

isAtomic :: HasAtomicity a => a -> Bool
isAtomic x = case atomicity x of
  Atom -> True
  _ -> False
