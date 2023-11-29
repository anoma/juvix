module Juvix.Data.Fixity where

import Juvix.Data.NameId
import Juvix.Prelude.Base

-- | Note that the order of the constructors is important due to the `Ord`
-- instance.
data Precedence
  = PrecArrow
  | PrecNat Int
  | PrecApp
  | PrecUpdate
  deriving stock (Show, Eq, Data, Ord)

data UnaryAssoc = AssocPostfix
  deriving stock (Show, Eq, Ord, Data)

data BinaryAssoc
  = AssocNone
  | AssocLeft
  | AssocRight
  deriving stock (Show, Eq, Ord, Data)

data OperatorArity
  = OpUnary UnaryAssoc
  | OpBinary BinaryAssoc
  | OpNone
  deriving stock (Show, Eq, Ord, Data)

data Fixity = Fixity
  { _fixityPrecedence :: Precedence,
    _fixityArity :: OperatorArity,
    _fixityId :: Maybe NameId
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
  OpBinary AssocLeft -> True
  _ -> False

isRightAssoc :: Fixity -> Bool
isRightAssoc opInf = case opInf ^. fixityArity of
  OpBinary AssocRight -> True
  _ -> False

isPostfixAssoc :: Fixity -> Bool
isPostfixAssoc opInf = case opInf ^. fixityArity of
  OpUnary AssocPostfix -> True
  _ -> False

isBinary :: Fixity -> Bool
isBinary f = case f ^. fixityArity of
  OpBinary {} -> True
  OpUnary {} -> False
  OpNone -> False

isUnary :: Fixity -> Bool
isUnary = not . isBinary

letFixity :: Fixity
letFixity =
  Fixity
    { _fixityPrecedence = PrecArrow,
      _fixityArity = OpBinary AssocRight,
      _fixityId = Nothing
    }

appFixity :: Fixity
appFixity =
  Fixity
    { _fixityPrecedence = PrecApp,
      _fixityArity = OpBinary AssocLeft,
      _fixityId = Nothing
    }

funFixity :: Fixity
funFixity =
  Fixity
    { _fixityPrecedence = PrecArrow,
      _fixityArity = OpBinary AssocRight,
      _fixityId = Nothing
    }

updateFixity :: Fixity
updateFixity =
  Fixity
    { _fixityPrecedence = PrecUpdate,
      _fixityArity = OpUnary AssocPostfix,
      _fixityId = Nothing
    }

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

isAtomic :: (HasAtomicity a) => a -> Bool
isAtomic x = case atomicity x of
  Atom -> True
  _ -> False
