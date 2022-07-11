module Juvix.Syntax.Concrete.Literal where

import Juvix.Prelude
import Juvix.Syntax.Fixity
import Juvix.Syntax.Loc
import Prettyprinter

type LiteralLoc = WithLoc Literal

data Literal
  = LitString Text
  | LitInteger Integer
  deriving stock (Show, Eq, Ord, Generic)

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
