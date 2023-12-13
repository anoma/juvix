module Juvix.Compiler.Nockma.Language where

import GHC.Base (Type)
import Juvix.Prelude hiding (Atom)

data Term a
  = TermAtom (Atom a)
  | TermCell (Cell a)
  deriving stock (Show, Eq)

data Cell a = Cell
  { _cellLeft :: Term a,
    _cellRight :: Term a
  }
  deriving stock (Show, Eq)

newtype Atom a = Atom
  { _atom :: a
  }
  deriving stock (Show, Eq)

data NockOp
  = OpAddress
  | OpQuote
  | OpApply
  | OpIsCell
  | OpInc
  | OpEq
  | OpIf
  | OpSequence
  | OpPush
  | OpCall
  | OpReplace
  | OpHint

data OperatorCell a = OperatorCell
  { _operatorCellOp :: NockOp,
    _operatorCellTerm :: Term a
  }

data AutoConsCell a = AutoConsCell
  { _autoConsCellLeft :: Cell a,
    _autoConsCellRight :: Term a
  }

data ParsedCell a
  = ParsedOperatorCell (OperatorCell a)
  | ParsedAutoConsCell (AutoConsCell a)

makeLenses ''Cell
makeLenses ''Atom
makeLenses ''OperatorCell
makeLenses ''AutoConsCell

class (Eq a) => NockNatural a where
  data ErrNockNaturalDecoding a :: Type
  nockNatural :: (Member (Error (ErrNockNaturalDecoding a)) r) => Atom a -> Sem r Natural
  nockTrue :: Atom a
  nockFalse :: Atom a
  nockSucc :: Atom a -> Atom a

instance NockNatural Natural where
  data ErrNockNaturalDecoding Natural = Void
  nockNatural a = return (a ^. atom)
  nockTrue = Atom 0
  nockFalse = Atom 1
  nockSucc = over atom succ
