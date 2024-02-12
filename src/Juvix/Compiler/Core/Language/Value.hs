module Juvix.Compiler.Core.Language.Value where

import Juvix.Compiler.Core.Language.Base
import Juvix.Compiler.Core.Language.Nodes

data ConstrApp = ConstrApp
  { _constrAppName :: Text,
    _constrAppFixity :: Irrelevant (Maybe Fixity),
    _constrAppArgs :: [Value]
  }
  deriving stock (Eq)

-- | Specifies Core values for user-friendly pretty printing.
data Value
  = ValueConstrApp ConstrApp
  | ValueConstant ConstantValue
  | ValueWildcard
  | ValueFun
  | ValueType
  deriving stock (Eq)

makeLenses ''ConstrApp

instance HasAtomicity ConstrApp where
  atomicity ConstrApp {..}
    | null _constrAppArgs = Atom
    | otherwise = Aggregate (fromMaybe appFixity (_constrAppFixity ^. unIrrelevant))

instance HasAtomicity Value where
  atomicity = \case
    ValueConstrApp x -> atomicity x
    ValueConstant {} -> Atom
    ValueWildcard -> Atom
    ValueFun -> Atom
    ValueType -> Atom
