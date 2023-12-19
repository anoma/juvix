module Juvix.Compiler.Nockma.Language where

import Data.HashMap.Strict qualified as HashMap
import GHC.Base (Type)
import Juvix.Prelude hiding (Atom, Path)
import Juvix.Prelude.Pretty

data ReplStatement a
  = ReplStatementExpression (ReplExpression a)
  | ReplStatementAssignment (Assignment a)

data ReplExpression a
  = ReplExpressionTerm (ReplTerm a)
  | ReplExpressionWithStack (WithStack a)

data WithStack a = WithStack
  { _withStackStack :: ReplTerm a,
    _withStackTerm :: ReplTerm a
  }

data ReplTerm a
  = ReplName Text
  | ReplTerm (Term a)

newtype Program a = Program
  { _programStatements :: [Statement a]
  }

data Statement a
  = StatementAssignment (Assignment a)
  | StatementStandalone (Term a)

data Assignment a = Assignment
  { _assignmentName :: Text,
    _assignmentBody :: Term a
  }

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
  | AtomHintPath
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

type EncodedPath = Natural

data Direction
  = L
  | R
  deriving stock (Show)

type Path = [Direction]

emptyPath :: Path
emptyPath = []

makeLenses ''Cell
makeLenses ''Atom
makeLenses ''OperatorCell
makeLenses ''AutoConsCell
makeLenses ''Program
makeLenses ''Assignment
makeLenses ''WithStack

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

decodePath :: forall r. (Member Fail r) => EncodedPath -> Sem r Path
decodePath ep = execOutputList (go ep)
  where
    go :: EncodedPath -> Sem (Output Direction ': r) ()
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

serializePathNatural :: Path -> Natural
serializePathNatural = foldl' step 1
  where
    step :: Natural -> Direction -> Natural
    step n = \case
      R -> 2 * n + 1
      L -> 2 * n

class (Eq a) => NockNatural a where
  type ErrNockNatural a :: Type
  nockNatural :: (Member (Error (ErrNockNatural a)) r) => Atom a -> Sem r Natural
  serializeNockOp :: NockOp -> a
  serializePath :: Path -> a

  errInvalidOp :: Atom a -> ErrNockNatural a

  errInvalidPath :: Atom a -> ErrNockNatural a

  nockOp :: (Member (Error (ErrNockNatural a)) r) => Atom a -> Sem r NockOp
  nockOp atm = do
    n <- nockNatural atm
    failWithError (errInvalidOp atm) (parseOp n)

  nockPath :: (Member (Error (ErrNockNatural a)) r) => Atom a -> Sem r Path
  nockPath atm = do
    n <- nockNatural atm
    failWithError (errInvalidPath atm) (decodePath n)

  nockTrue :: Atom a
  nockFalse :: Atom a
  nockSucc :: Atom a -> Atom a

data NockNaturalNaturalError
  = NaturalInvalidPath (Atom Natural)
  | NaturalInvalidOp (Atom Natural)
  deriving stock (Show)

instance NockNatural Natural where
  type ErrNockNatural Natural = NockNaturalNaturalError
  nockNatural a = return (a ^. atom)
  nockTrue = Atom 0 (Irrelevant (Just AtomHintBool))
  nockFalse = Atom 1 (Irrelevant (Just AtomHintBool))
  nockSucc = over atom succ
  errInvalidOp atm = NaturalInvalidOp atm
  errInvalidPath atm = NaturalInvalidPath atm
  serializeNockOp = serializeOp
  serializePath = serializePathNatural

class IsNock nock where
  toNock :: nock -> Term Natural

instance IsNock (Term Natural) where
  toNock = id

instance IsNock (Atom Natural) where
  toNock = TermAtom

instance IsNock (Cell Natural) where
  toNock = TermCell

instance IsNock Natural where
  toNock n = toNock (Atom n (Irrelevant Nothing))

instance IsNock NockOp where
  toNock op = toNock (Atom (serializeOp op) (Irrelevant (Just AtomHintOp)))

instance IsNock Bool where
  toNock = \case
    False -> toNock (nockFalse @Natural)
    True -> toNock (nockTrue @Natural)

instance IsNock Path where
  toNock pos = toNock (Atom (serializePathNatural pos) (Irrelevant (Just AtomHintPath)))

infixr 5 #

(#) :: (IsNock x, IsNock y) => x -> y -> Term Natural
a # b = TermCell (Cell (toNock a) (toNock b))
