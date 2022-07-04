module MiniJuvix.Syntax.Concrete.Literal where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Fixity
import Prettyprinter

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
