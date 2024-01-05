module Juvix.Compiler.Nockma.Evaluator
  ( module Juvix.Compiler.Nockma.Evaluator,
    module Juvix.Compiler.Nockma.Evaluator.Error,
  )
where

import Juvix.Compiler.Nockma.Evaluator.Error
import Juvix.Compiler.Nockma.Language
import Juvix.Prelude hiding (Atom, Path)

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

asPath :: (Members '[Error NockEvalError, Error (ErrNockNatural a)] r, NockNatural a) => Term a -> Sem r Path
asPath = asAtom >=> nockPath

subTermT' :: Path -> Traversal (Term a) (Term a) (First (Term a)) (Term a)
subTermT' pos f = subTermT pos (f . First . Just)

subTermT :: Path -> Traversal' (Term a) (Term a)
subTermT = go
  where
    go :: Path -> (forall f. (Applicative f) => (Term a -> f (Term a)) -> Term a -> f (Term a))
    go = \case
      [] -> id
      d : ds -> \g t -> case t of
        TermAtom {} -> pure t
        TermCell c -> case d of
          L -> (\l' -> TermCell (set cellLeft l' c)) <$> go ds g (c ^. cellLeft)
          R -> (\r' -> TermCell (set cellRight r' c)) <$> go ds g (c ^. cellRight)

subTerm :: (Member (Error NockEvalError) r) => Term a -> Path -> Sem r (Term a)
subTerm term pos = do
  case term ^? subTermT pos of
    Nothing -> throw InvalidPath
    Just t -> return t

setSubTerm :: (Member (Error NockEvalError) r) => Term a -> Path -> Term a -> Sem r (Term a)
setSubTerm term pos repTerm =
  let (old, new) = setAndRemember (subTermT' pos) repTerm term
   in if
          | isNothing (getFirst old) -> throw InvalidPath
          | otherwise -> return new

parseCell :: forall r a. (Members '[Error NockEvalError, Error (ErrNockNatural a)] r, NockNatural a) => Cell a -> Sem r (ParsedCell a)
parseCell c = case c ^. cellLeft of
  TermAtom a -> ParsedOperatorCell <$> parseOperatorCell a (c ^. cellRight)
  TermCell l -> return (ParsedAutoConsCell (AutoConsCell l (c ^. cellRight)))
  where
    parseOperatorCell :: Atom a -> Term a -> Sem r (OperatorCell a)
    parseOperatorCell a t = do
      op <- nockOp a
      return
        OperatorCell
          { _operatorCellOp = op,
            _operatorCellTerm = t
          }

fromReplTerm :: (Members '[Error NockEvalError] r) => HashMap Text (Term a) -> ReplTerm a -> Sem r (Term a)
fromReplTerm namedTerms = \case
  ReplName n -> maybe (throw (AssignmentNotFound n)) return (namedTerms ^. at n)
  ReplTerm t -> return t

programAssignments :: Maybe (Program a) -> HashMap Text (Term a)
programAssignments mprog =
  hashMap
    [ (as ^. assignmentName, as ^. assignmentBody)
      | StatementAssignment as <- mprog ^. _Just . programStatements
    ]

-- | The stack provided in the replExpression has priority
evalRepl ::
  forall r a.
  (Members '[Error NockEvalError, Error (ErrNockNatural a)] r, NockNatural a) =>
  Maybe (Program a) ->
  Maybe (Term a) ->
  ReplExpression a ->
  Sem r (Term a)
evalRepl mprog defaultStack expr = do
  (mstack, t) <- case expr of
    ReplExpressionTerm tm -> return (defaultStack, tm)
    ReplExpressionWithStack w -> do
      t' <- fromReplTerm namedTerms (w ^. withStackStack)
      return (Just t', w ^. withStackTerm)
  stack <- maybe errNoStack return mstack
  fromReplTerm namedTerms t >>= eval stack
  where
    errNoStack :: Sem r x
    errNoStack = throw NoStack

    namedTerms :: HashMap Text (Term a)
    namedTerms = programAssignments mprog

eval ::
  forall r a.
  (Members '[Error NockEvalError, Error (ErrNockNatural a)] r, NockNatural a) =>
  Term a ->
  Term a ->
  Sem r (Term a)
eval stack = \case
  TermAtom {} -> throw ExpectedCell
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
        goOpAddress = asPath (c ^. operatorCellTerm) >>= subTerm stack

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
          Cell rot1 t2 <- asCell (c ^. operatorCellTerm)
          Cell ro t1 <- asCell rot1
          r <- asPath ro
          t1' <- eval stack t1
          t2' <- eval stack t2
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
          r <- asPath (cellTerm ^. cellLeft)
          t' <- eval stack (cellTerm ^. cellRight)
          subTerm t' r >>= eval t'

        goOpSequence :: Sem r (Term a)
        goOpSequence = do
          cellTerm <- asCell (c ^. operatorCellTerm)
          t1' <- eval stack (cellTerm ^. cellLeft)
          eval t1' (cellTerm ^. cellRight)
