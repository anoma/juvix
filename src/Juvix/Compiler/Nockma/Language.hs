module Juvix.Compiler.Nockma.Language where

import Data.HashMap.Strict qualified as HashMap
import GHC.Base (Type)
import Juvix.Prelude hiding (Atom)
import Juvix.Prelude.Pretty

data Term a
  = TermAtom (Atom a)
  | TermCell (Cell a)
  deriving stock (Show, Eq)

data Cell a = Cell
  { _cellLeft :: Term a,
    _cellRight :: Term a
  }
  deriving stock (Show, Eq)

data Atom a = Atom
  { _atom :: a,
    _atomInfo :: Irrelevant (Maybe AtomHint)
  }
  deriving stock (Show, Eq)

data AtomHint
  = AtomHintOp
  | AtomHintPosition
  | AtomHintBool

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
  deriving stock (Bounded, Enum, Eq, Generic)

instance Hashable NockOp

instance Pretty NockOp where
  pretty = \case
    OpAddress -> "@"
    OpQuote -> "quote"
    OpApply -> "apply"
    OpIsCell -> "isCell"
    OpInc -> "suc"
    OpEq -> "="
    OpIf -> "if"
    OpSequence -> "seq"
    OpPush -> "push"
    OpCall -> "call"
    OpReplace -> "replace"
    OpHint -> "hint"

atomOps :: HashMap Text NockOp
atomOps = HashMap.fromList [(prettyText op, op) | op <- allElements]

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

type EncodedPosition = Natural

data Direction = L | R
  deriving stock (Show)

newtype Position = Position {_positionDirections :: [Direction]}
  deriving stock (Show)

makeLenses ''Cell
makeLenses ''Atom
makeLenses ''OperatorCell
makeLenses ''AutoConsCell
makeLenses ''Position

naturalNockOps :: HashMap Natural NockOp
naturalNockOps = HashMap.fromList [(serializeOp op, op) | op <- allElements]

nockOpsNatural :: HashMap NockOp Natural
nockOpsNatural = HashMap.fromList (swap <$> HashMap.toList naturalNockOps)

parseOp :: (Member Fail r) => Natural -> Sem r NockOp
parseOp n = failMaybe (naturalNockOps ^. at n)

serializeOp :: NockOp -> Natural
serializeOp = \case
  OpAddress -> 0
  OpQuote -> 1
  OpApply -> 2
  OpIsCell -> 3
  OpInc -> 4
  OpEq -> 5
  OpIf -> 6
  OpSequence -> 7
  OpPush -> 8
  OpCall -> 9
  OpReplace -> 10
  OpHint -> 11

decodePosition :: forall r. (Member Fail r) => EncodedPosition -> Sem r Position
decodePosition ep = Position <$> execOutputList (go ep)
  where
    go :: EncodedPosition -> Sem (Output Direction ': r) ()
    go = \case
      0 -> fail
      1 -> return ()
      x ->
        if
            | even x -> do
                go (x `div` 2)
                output L
            | otherwise -> do
                go ((x - 1) `div` 2)
                output R

class (Eq a) => NockNatural a where
  type ErrNockNatural a :: Type
  nockNatural :: (Member (Error (ErrNockNatural a)) r) => Atom a -> Sem r Natural
  serializeNockOp :: NockOp -> a

  errInvalidOp :: Atom a -> ErrNockNatural a

  errInvalidPosition :: Atom a -> ErrNockNatural a

  nockOp :: (Member (Error (ErrNockNatural a)) r) => Atom a -> Sem r NockOp
  nockOp atm = do
    n <- nockNatural atm
    failWithError (errInvalidOp atm) (parseOp n)

  nockPosition :: (Member (Error (ErrNockNatural a)) r) => Atom a -> Sem r Position
  nockPosition atm = do
    n <- nockNatural atm
    failWithError (errInvalidPosition atm) (decodePosition n)

  nockTrue :: Atom a
  nockFalse :: Atom a
  nockSucc :: Atom a -> Atom a

data NockNaturalNaturalError
  = NaturalInvalidPosition (Atom Natural)
  | NaturalInvalidOp (Atom Natural)

instance NockNatural Natural where
  type ErrNockNatural Natural = NockNaturalNaturalError
  nockNatural a = return (a ^. atom)
  nockTrue = Atom 0 (Irrelevant (Just AtomHintBool))
  nockFalse = Atom 1 (Irrelevant (Just AtomHintBool))
  nockSucc = over atom succ
  errInvalidOp atm = NaturalInvalidOp atm
  errInvalidPosition atm = NaturalInvalidPosition atm
  serializeNockOp = serializeOp
