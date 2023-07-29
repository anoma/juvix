module Juvix.Data.Fixity where

import Juvix.Prelude.Base

-- | Note that the order of the constructors is important due to the `Ord`
-- instance.
-- TODO should we rename PrecMinusOmega to PrecApp, PrecMinusOmega1 to PrecUpdate, and PrecOmega to PrecFunction/Arrow?
data Precedence
  = PrecMinusOmega1
  | PrecMinusOmega
  | PrecNat Natural
  | PrecOmega
  deriving stock (Show, Eq, Data, Ord)

data UnaryAssoc = AssocPostfix
  deriving stock (Show, Eq, Ord, Data)

data BinaryAssoc
  = AssocNone
  | AssocLeft
  | AssocRight
  deriving stock (Show, Eq, Ord, Data)

data OperatorArity
  = Unary UnaryAssoc
  | Binary BinaryAssoc
  deriving stock (Show, Eq, Ord, Data)

data Fixity = Fixity
  { _fixityPrecedence :: Precedence,
    _fixityArity :: OperatorArity
  }
  deriving stock (Show, Eq, Ord, Data)

makeLenses ''Fixity

data Atomicity
  = Atom
  | Aggregate Fixity
  deriving stock (Eq)

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

isBinary :: Fixity -> Bool
isBinary f = case f ^. fixityArity of
  Binary {} -> True
  Unary {} -> False

isUnary :: Fixity -> Bool
isUnary = not . isBinary

appFixity :: Fixity
appFixity = Fixity PrecOmega (Binary AssocLeft)

funFixity :: Fixity
funFixity = Fixity PrecMinusOmega (Binary AssocRight)

updateFixity :: Fixity
updateFixity = Fixity PrecMinusOmega1 (Unary AssocPostfix)

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
