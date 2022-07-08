module Juvix.Syntax.Concrete.LiteralLoc
  ( module Juvix.Syntax.Concrete.LiteralLoc,
    module Juvix.Syntax.Concrete.Literal,
  )
where

import Juvix.Prelude
import Juvix.Prelude.Pretty
import Juvix.Syntax.Concrete.Literal
import Juvix.Syntax.Fixity

data LiteralLoc = LiteralLoc
  { _literalLocLiteral :: Literal,
    _literalLocLoc :: Interval
  }
  deriving stock (Show, Generic)

makeLenses ''LiteralLoc

instance Hashable LiteralLoc

instance HasAtomicity LiteralLoc where
  atomicity = atomicity . (^. literalLocLiteral)

instance Pretty LiteralLoc where
  pretty = pretty . (^. literalLocLiteral)

instance Eq LiteralLoc where
  l1 == l2 = l1 ^. literalLocLiteral == l2 ^. literalLocLiteral

instance HasLoc LiteralLoc where
  getLoc = (^. literalLocLoc)

deriving stock instance Ord LiteralLoc
