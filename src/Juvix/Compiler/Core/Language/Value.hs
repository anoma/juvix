module Juvix.Compiler.Core.Language.Value where

import Juvix.Compiler.Core.Language.Base
import Juvix.Compiler.Core.Language.Nodes

data ConstrApp = ConstrApp
  { _constrAppName :: Text,
    _constrAppFixity :: Maybe Fixity,
    _constrAppArgs :: [Value]
  }

-- | Specifies Core values for user-friendly pretty printing.
data Value
  = ValueConstrApp ConstrApp
  | ValueConstant ConstantValue
  | ValueWildcard
  | ValueFun
  | ValueType

makeLenses ''ConstrApp

instance HasAtomicity ConstrApp where
  atomicity ConstrApp {..}
    | null _constrAppArgs = Atom
    | otherwise = Aggregate (fromMaybe appFixity _constrAppFixity)

instance HasAtomicity Value where
  atomicity = \case
    ValueConstrApp x -> atomicity x
    ValueConstant {} -> Atom
    ValueWildcard -> Atom
    ValueFun -> Atom
    ValueType -> Atom
