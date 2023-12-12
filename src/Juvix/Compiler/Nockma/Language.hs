module Juvix.Compiler.Nockma.Language where

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

type NockTerm = Term Integer

-- type NockmaTerm = Term ...

type EncodedPosition = Natural

data Direction = L | R
  deriving stock (Show)

type Position = [Direction]

data NockEvalError
  = InvalidPosition
  | InvalidEncodedPosition
  | InvalidOpCode
  | ExpectedAtom
  | ExpectedCell
  deriving stock (Show)

asAtom :: (Member (Error NockEvalError) r) => Term a -> Sem r (Atom a)
asAtom = \case
  TermAtom a -> return a
  TermCell {} -> throw ExpectedAtom

asCell :: (Member (Error NockEvalError) r) => Term a -> Sem r (Cell a)
asCell = \case
  TermAtom {} -> throw ExpectedCell
  TermCell c -> return c

asBool :: (Member (Error NockEvalError) r, NockNatural a) => Term a -> Sem r Bool
asBool t = do
  a <- asAtom t
  return (a == nockTrue)

asPosition :: (Members '[Error NockEvalError, Error (ErrNockNaturalDecoding a)] r, NockNatural a) => Term a -> Sem r Position
asPosition = asAtom >=> nockNatural >=> decodePosition

decodePosition :: forall r. (Member (Error NockEvalError) r) => EncodedPosition -> Sem r Position
decodePosition ep = execOutputList (go ep)
  where
    go :: EncodedPosition -> Sem (Output Direction ': r) ()
    go = \case
      0 -> throw InvalidEncodedPosition
      1 -> return ()
      x ->
        if
            | even x -> do
                go (x `div` 2)
                output L
            | otherwise -> do
                go ((x - 1) `div` 2)
                output R

subTermT' :: Position -> Traversal (Term a) (Term a) (First (Term a)) (Term a)
subTermT' pos f = subTermT pos (f . First . Just)

subTermT :: Position -> Traversal' (Term a) (Term a)
subTermT = go
  where
    go :: Position -> (forall f. (Applicative f) => (Term a -> f (Term a)) -> Term a -> f (Term a))
    go = \case
      [] -> id
      d : ds -> \g t -> case t of
        TermAtom {} -> pure t
        TermCell c -> case d of
          L -> (\l' -> TermCell (set cellLeft l' c)) <$> go ds g (c ^. cellLeft)
          R -> (\r' -> TermCell (set cellRight r' c)) <$> go ds g (c ^. cellRight)

subTerm :: (Member (Error NockEvalError) r) => Term a -> Position -> Sem r (Term a)
subTerm term pos = do
  case term ^? subTermT pos of
    Nothing -> throw InvalidPosition
    Just t -> return t

setSubTerm :: (Member (Error NockEvalError) r) => Term a -> Position -> Term a -> Sem r (Term a)
setSubTerm term pos repTerm =
  let (old, new) = setAndRemember (subTermT' pos) repTerm term
   in if
          | isNothing (getFirst old) -> throw InvalidPosition
          | otherwise -> return new

newtype GenericNockEvalError = GenericNockEvalError
  { _genericNockEvalErrorMessage :: AnsiText
  }

class ToGenericNockEvalError a where
  toGenericNockEvalError :: a -> GenericNockEvalError

parseCell :: forall r a. (Members '[Error NockEvalError, Error (ErrNockNaturalDecoding a)] r, NockNatural a) => Cell a -> Sem r (ParsedCell a)
parseCell c = case c ^. cellLeft of
  TermAtom a -> ParsedOperatorCell <$> parseOperatorCell a (c ^. cellRight)
  TermCell l -> return (ParsedAutoConsCell (AutoConsCell l (c ^. cellRight)))
  where
    parseOperatorCell :: Atom a -> Term a -> Sem r (OperatorCell a)
    parseOperatorCell a t = do
      op <- parseOperator a
      return
        OperatorCell
          { _operatorCellOp = op,
            _operatorCellTerm = t
          }

    parseOperator :: Atom a -> Sem r NockOp
    parseOperator a = do
      n <- nockNatural a
      case n of
        0 -> return OpAddress
        1 -> return OpQuote
        2 -> return OpApply
        3 -> return OpIsCell
        4 -> return OpInc
        5 -> return OpEq
        6 -> return OpIf
        7 -> return OpSequence
        8 -> return OpPush
        9 -> return OpCall
        10 -> return OpReplace
        11 -> return OpHint
        _ -> throw InvalidOpCode

      return OpAddress

eval :: forall r a. (Members '[Error NockEvalError, Error (ErrNockNaturalDecoding a)] r, NockNatural a) => Term a -> Term a -> Sem r (Term a)
eval stack = \case
  a@TermAtom {} -> return a
  TermCell c -> do
    pc <- parseCell c
    case pc of
      ParsedAutoConsCell a -> goAutoConsCell a
      ParsedOperatorCell o -> goOperatorCell o
  where
    goAutoConsCell :: AutoConsCell a -> Sem r (Term a)
    goAutoConsCell c = do
      _cellLeft <- eval stack (TermCell (c ^. autoConsCellLeft))
      _cellRight <- eval stack (c ^. autoConsCellRight)
      return (TermCell Cell {..})

    goOperatorCell :: OperatorCell a -> Sem r (Term a)
    goOperatorCell c = case c ^. operatorCellOp of
      OpAddress -> goOpAddress
      OpQuote -> goOpQuote
      OpApply -> goOpApply
      OpIsCell -> goOpIsCell
      OpInc -> goOpInc
      OpEq -> goOpEq
      OpIf -> goOpIf
      OpSequence -> goOpSequence
      OpPush -> goOpPush
      OpCall -> goOpCall
      OpReplace -> goOpReplace
      OpHint -> goOpHint
      where
        goOpAddress :: Sem r (Term a)
        goOpAddress = asPosition (c ^. operatorCellTerm) >>= subTerm stack

        goOpQuote :: Sem r (Term a)
        goOpQuote = return (c ^. operatorCellTerm)

        goOpIsCell :: Sem r (Term a)
        goOpIsCell = return . TermAtom $ case c ^. operatorCellTerm of
          TermCell {} -> nockTrue
          TermAtom {} -> nockFalse

        goOpHint :: Sem r (Term a)
        goOpHint = do
          -- Ignore the hint and evaluate
          h <- asCell (c ^. operatorCellTerm)
          eval stack (h ^. cellRight)

        goOpPush :: Sem r (Term a)
        goOpPush = do
          cellTerm <- asCell (c ^. operatorCellTerm)
          l <- eval stack (cellTerm ^. cellLeft)
          let s = TermCell Cell {_cellLeft = l, _cellRight = stack}
          eval s (cellTerm ^. cellRight)

        goOpReplace :: Sem r (Term a)
        goOpReplace = do
          cellTerm <- asCell (c ^. operatorCellTerm)
          rt1 <- asCell (cellTerm ^. cellLeft)
          r <- asPosition (rt1 ^. cellLeft)
          let t1 = rt1 ^. cellRight
          t1' <- eval stack t1
          t2' <- eval stack (cellTerm ^. cellRight)
          setSubTerm t2' r t1'

        goOpApply :: Sem r (Term a)
        goOpApply = do
          cellTerm <- asCell (c ^. operatorCellTerm)
          t1' <- eval stack (cellTerm ^. cellLeft)
          t2' <- eval stack (cellTerm ^. cellRight)
          eval t1' t2'

        goOpIf :: Sem r (Term a)
        goOpIf = do
          cellTerm <- asCell (c ^. operatorCellTerm)
          let t0 = cellTerm ^. cellLeft
          Cell t1 t2 <- asCell (cellTerm ^. cellRight)
          cond <- eval stack t0 >>= asBool
          if
              | cond -> eval stack t1
              | otherwise -> eval stack t2

        goOpInc :: Sem r (Term a)
        goOpInc = TermAtom . nockSucc <$> (eval stack (c ^. operatorCellTerm) >>= asAtom)

        goOpEq :: Sem r (Term a)
        goOpEq = do
          cellTerm <- asCell (c ^. operatorCellTerm)
          l <- eval stack (cellTerm ^. cellLeft)
          r <- eval stack (cellTerm ^. cellRight)
          return . TermAtom $
            if
                | l == r -> nockTrue
                | otherwise -> nockFalse

        goOpCall :: Sem r (Term a)
        goOpCall = do
          cellTerm <- asCell (c ^. operatorCellTerm)
          r <- asPosition (cellTerm ^. cellLeft)
          t' <- eval stack (cellTerm ^. cellRight)
          subTerm t' r >>= eval t'

        goOpSequence :: Sem r (Term a)
        goOpSequence = do
          cellTerm <- asCell (c ^. operatorCellTerm)
          t1' <- eval stack (cellTerm ^. cellLeft)
          eval t1' (cellTerm ^. cellRight)
