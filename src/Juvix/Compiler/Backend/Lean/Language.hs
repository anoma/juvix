module Juvix.Compiler.Backend.Lean.Language 
  ( module Juvix.Compiler.Backend.Lean.Language,
    module Juvix.Compiler.Internal.Data.Name,
    module Juvix.Prelude,
  )
where

import Juvix.Compiler.Internal.Data.Name hiding (letFixity)
import Juvix.Prelude hiding (Cons, letFixity)

data Expression
  = Unit
  deriving stock (Eq)

data Declaration
  = Def Name Expression -- Definitions
  | Theorem Name Expression -- Theorems
  deriving stock (Eq)

data Module = Module
  { _moduleName :: Name, -- Name of the Lean module
    _moduleImports :: [Name],
    _moduleDeclarations :: [Declaration] -- Declarations within the module
  }
  deriving stock (Eq)

makeLenses ''Module
