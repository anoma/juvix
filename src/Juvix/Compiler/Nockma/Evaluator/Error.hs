module Juvix.Compiler.Nockma.Evaluator.Error
  ( module Juvix.Compiler.Nockma.Evaluator.Error,
    module Juvix.Compiler.Nockma.Evaluator.Crumbs,
  )
where

import Juvix.Compiler.Nockma.Evaluator.Crumbs
import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Pretty.Base
import Juvix.Prelude hiding (Atom, Path)

data NockEvalError a
  = ErrInvalidPath (InvalidPath a)
  | ErrExpectedAtom (ExpectedAtom a)
  | ErrExpectedCell (ExpectedCell a)
  | -- TODO perhaps this should be a repl error type
    ErrNoStack NoStack
  | -- TODO perhaps this should be a repl error type
    ErrAssignmentNotFound Text

newtype GenericNockEvalError = GenericNockEvalError
  { _genericNockEvalErrorMessage :: AnsiText
  }

class ToGenericNockEvalError a where
  toGenericNockEvalError :: a -> GenericNockEvalError

data ExpectedCell a = ExpectedCell
  { _expectedCellCtx :: EvalCtx,
    _expectedCellAtom :: Atom a
  }

data ExpectedAtom a = ExpectedAtom
  { _expectedAtomCtx :: EvalCtx,
    _expectedAtomCell :: Cell a
  }

data InvalidPath a = InvalidPath
  { _invalidPathCtx :: EvalCtx,
    _invalidPathTerm :: Term a,
    _invalidPathPath :: Path
  }

data NoStack = NoStack

throwInvalidPath :: (Members '[Error (NockEvalError a), Reader EvalCtx] r) => Term a -> Path -> Sem r x
throwInvalidPath tm p = do
  ctx <- ask
  throw $
    ErrInvalidPath
      InvalidPath
        { _invalidPathCtx = ctx,
          _invalidPathTerm = tm,
          _invalidPathPath = p
        }

throwExpectedCell :: (Members '[Error (NockEvalError a), Reader EvalCtx] r) => Atom a -> Sem r x
throwExpectedCell a = do
  ctx <- ask
  throw $
    ErrExpectedCell
      ExpectedCell
        { _expectedCellCtx = ctx,
          _expectedCellAtom = a
        }

throwExpectedAtom :: (Members '[Error (NockEvalError a), Reader EvalCtx] r) => Cell a -> Sem r x
throwExpectedAtom a = do
  ctx <- ask
  throw $
    ErrExpectedAtom
      ExpectedAtom
        { _expectedAtomCtx = ctx,
          _expectedAtomCell = a
        }

instance PrettyCode NoStack where
  ppCode _ = return "Missing stack"

instance (PrettyCode a, NockNatural a) => PrettyCode (InvalidPath a) where
  ppCode InvalidPath {..} = do
    ctx <- ppCtx _invalidPathCtx
    path <- ppCode _invalidPathPath
    tm <- ppCode _invalidPathTerm
    return (ctx <> "The path" <+> path <+> "is invalid for the following term:" <> line <> tm)

instance (PrettyCode a, NockNatural a) => PrettyCode (ExpectedAtom a) where
  ppCode ExpectedAtom {..} = do
    cell <- ppCode _expectedAtomCell
    ctx <- ppCtx _expectedAtomCtx
    let atm = annotate AnnImportant "atom"
    return (ctx <> "Expected an" <+> atm <+> "but got:" <> line <> cell)

instance (PrettyCode a, NockNatural a) => PrettyCode (ExpectedCell a) where
  ppCode ExpectedCell {..} = do
    atm <- ppCode _expectedCellAtom
    ctx <- ppCtx _expectedCellCtx
    let cell = annotate AnnImportant "cell"
    return (ctx <> "Expected an" <+> atm <+> "but got:" <> line <> cell)

instance (PrettyCode a, NockNatural a) => PrettyCode (NockEvalError a) where
  ppCode = \case
    ErrInvalidPath e -> ppCode e
    ErrExpectedAtom e -> ppCode e
    ErrExpectedCell e -> ppCode e
    ErrNoStack e -> ppCode e
    ErrAssignmentNotFound e -> return (pretty e)
