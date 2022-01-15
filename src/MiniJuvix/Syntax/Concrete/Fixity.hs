module MiniJuvix.Syntax.Concrete.Fixity where

import MiniJuvix.Utils.Prelude
import Language.Haskell.TH.Syntax (Lift)

type Precedence = Natural

data UnaryAssoc = AssocPrefix | AssocPostfix
  deriving stock (Show, Eq, Ord, Lift)

data BinaryAssoc = AssocNone | AssocLeft | AssocRight
  deriving stock (Show, Eq, Ord, Lift)

data OperatorArity
  = Unary
      { unaryAssoc :: UnaryAssoc
      }
  | Binary
      { binaryAssoc :: BinaryAssoc
      }
  deriving stock (Show, Eq, Ord, Lift)

data Fixity = Fixity
  { fixityPrecedence :: Precedence,
    fixityArity :: OperatorArity
  }
  deriving stock (Show, Eq, Ord, Lift)

