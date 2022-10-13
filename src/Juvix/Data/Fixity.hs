module Juvix.Data.Fixity where

import Juvix.Prelude.Base

data Precedence
  = PrecMinusOmega
  | PrecNat Natural
  | PrecOmega
  deriving stock (Show, Eq)

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
  deriving stock (Show, Eq, Ord)

data BinaryAssoc
  = AssocNone
  | AssocLeft
  | AssocRight
  deriving stock (Show, Eq, Ord)

data OperatorArity
  = Unary UnaryAssoc
  | Binary BinaryAssoc
  deriving stock (Show, Eq, Ord)

data Fixity = Fixity
  { _fixityPrecedence :: Precedence,
    _fixityArity :: OperatorArity
  }
  deriving stock (Show, Eq, Ord)

makeLenses ''Fixity

data Atomicity
  = Atom
  | Aggregate Fixity

class HasAtomicity a where
  atomicity :: a -> Atomicity

class HasFixity a where
  getFixity :: a -> Fixity

isLeftAssoc :: Fixity -> Bool
isLeftAssoc opInf = case opInf ^. fixityArity of
  Binary AssocLeft -> True
  _ -> False

isRightAssoc :: Fixity -> Bool
isRightAssoc opInf = case opInf ^. fixityArity of
  Binary AssocRight -> True
  _ -> False

isPostfixAssoc :: Fixity -> Bool
isPostfixAssoc opInf = case opInf ^. fixityArity of
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
    | argPrec > opPrec -> False
    | argPrec < opPrec -> True
    | associates opInf -> False
    | otherwise -> True
    where
      argPrec :: Precedence
      argPrec = argInf ^. fixityPrecedence
      opPrec :: Precedence
      opPrec = opInf ^. fixityPrecedence

isAtomic :: HasAtomicity a => a -> Bool
isAtomic x = case atomicity x of
  Atom -> True
  _ -> False
