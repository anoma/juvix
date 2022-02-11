{-# OPTIONS_GHC -Wno-typed-holes #-}
module MiniJuvix.Syntax.Concrete.Scoped.Error where

import MiniJuvix.Utils.Prelude
import MiniJuvix.Syntax.Concrete.Language
import MiniJuvix.Syntax.Concrete.Scoped.Scope
import Prettyprinter

data ScopeError
  = ErrParser Text
  | ErrInfixParser Text
  | ErrInfixPattern Text
  | ErrAlreadyDefined AlreadyDefined
  | ErrLacksTypeSig Symbol
  | ErrImportCycle TopModulePath
  | ErrOpenNotInScope QualifiedName
  | ErrSymNotInScope Symbol Scope LocalVars
  | ErrQualSymNotInScope QualifiedName
  | ErrModuleNotInScope Name
  | ErrBindGroup Symbol
  | ErrDuplicateFixity Symbol
  | ErrMultipleExport Symbol
  | ErrAmbiguousSym [SymbolEntry]
  | ErrAmbiguousModuleSym [SymbolEntry]
  -- | Eventually this needs to go away
  | ErrGeneric Text
  deriving stock (Show)

data EAnn = Highlight

ppScopeError :: ScopeError -> Doc EAnn
ppScopeError e = case e of
  ErrParser txt -> pretty txt
  ErrGeneric txt -> pretty txt
  ErrInfixParser txt -> pretty txt
  ErrInfixPattern txt -> pretty txt
  ErrAlreadyDefined sym -> undefined
  ErrLacksTypeSig sym -> undefined
  ErrImportCycle tmp -> undefined
  ErrOpenNotInScope qn -> undefined
  ErrSymNotInScope sym sc lv -> undefined
  ErrQualSymNotInScope qn -> undefined
  ErrModuleNotInScope na -> undefined
  ErrBindGroup sym -> undefined
  ErrDuplicateFixity sym -> undefined
  ErrMultipleExport sym -> undefined
  ErrAmbiguousSym ses -> undefined
  ErrAmbiguousModuleSym ses -> undefined

data AlreadyDefined = AlreadyDefined {
  _alreadyDefinedSymbol :: Symbol,
  _alreadyDefinedLoc :: Symbol
  }
 deriving stock (Show)
