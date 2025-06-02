module Juvix.Compiler.Concrete.Data.Literal
  ( module Juvix.Compiler.Concrete.Data.Literal,
    module Juvix.Data.IntegerWithBase,
  )
where

import Juvix.Data.Fixity
import Juvix.Data.IntegerWithBase
import Juvix.Extra.Serialize
import Juvix.Prelude
import Prettyprinter

data Literal
  = LitString Text
  | LitIntegerWithBase IntegerWithBase
  deriving stock (Show, Eq, Ord, Generic, Data)

type LiteralLoc = WithLoc Literal

instance Hashable Literal

instance Serialize Literal

instance NFData Literal

instance HasAtomicity Literal where
  atomicity = \case
    LitString {} -> Atom
    LitIntegerWithBase h -> atomicity h

instance Pretty Literal where
  pretty = \case
    LitIntegerWithBase n -> pretty n
    LitString s -> ppStringLit s
      where
        ppStringLit :: Text -> Doc a
        ppStringLit = dquotes . pretty
