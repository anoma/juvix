module Juvix.Compiler.Concrete.Data.Literal where

import Juvix.Data.Fixity
import Juvix.Prelude
import Prettyprinter

type LiteralLoc = WithLoc Literal

data Literal
  = LitString Text
  | LitInteger Integer
  deriving stock (Show, Eq, Ord, Generic, Data)

instance Hashable Literal

instance HasAtomicity Literal where
  atomicity = \case
    LitInteger {} -> Atom
    LitString {} -> Atom

instance Pretty Literal where
  pretty = \case
    LitInteger n -> pretty n
    LitString s -> ppStringLit s
      where
        ppStringLit :: Text -> Doc a
        ppStringLit = dquotes . pretty
