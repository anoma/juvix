module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Positivity.ConstructorArg.Base where

import Juvix.Compiler.Internal.Language
import Juvix.Prelude

data ConstructorArg
  = ConstructorArgFun Fun
  | ConstructorArgApp App

data Fun = Fun
  { _funLeft :: ConstructorArg,
    _funRight :: ConstructorArg
  }

data AppLhs
  = AppVar VarName
  | AppAxiom AxiomName
  | AppInductive InductiveName
  deriving stock (Eq, Show, Generic)

instance Hashable AppLhs

data App = App
  { _appLhs :: AppLhs,
    _appArgs :: [ConstructorArg]
  }
