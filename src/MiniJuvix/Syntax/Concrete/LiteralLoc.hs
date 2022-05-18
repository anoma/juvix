module MiniJuvix.Syntax.Concrete.LiteralLoc
  ( module MiniJuvix.Syntax.Concrete.LiteralLoc,
    module MiniJuvix.Syntax.Concrete.Literal,
  )
where

import MiniJuvix.Prelude
import MiniJuvix.Prelude.Pretty
import MiniJuvix.Syntax.Concrete.Literal
import MiniJuvix.Syntax.Fixity

data LiteralLoc = LiteralLoc
  { _literalLocLiteral :: Literal,
    _literalLocLoc :: Interval
  }
  deriving stock (Show)

makeLenses ''LiteralLoc

instance HasAtomicity LiteralLoc where
  atomicity = atomicity . (^. literalLocLiteral)

instance Pretty LiteralLoc where
  pretty = pretty . (^. literalLocLiteral)

instance Eq LiteralLoc where
  l1 == l2 = l1 ^. literalLocLiteral == l2 ^. literalLocLiteral

instance HasLoc LiteralLoc where
  getLoc = (^. literalLocLoc)

deriving stock instance Ord LiteralLoc
