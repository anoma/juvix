module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Data.Context where

import Juvix.Compiler.Concrete.Data.Scope
import Juvix.Compiler.Concrete.Data.ScopedName qualified as Scoped
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromSource.Data.Context qualified as Parsed
import Juvix.Compiler.Store.Scoped.Language
import Juvix.Prelude

data ScoperResult = ScoperResult
  { _resultParserResult :: Parsed.ParserResult,
    _resultModule :: Module 'Scoped 'ModuleTop,
    _resultScopedModule :: ScopedModule,
    _resultExports :: HashSet NameId,
    _resultScope :: HashMap TopModulePath Scope,
    _resultScoperState :: ScoperState
  }

makeLenses ''ScoperResult

mainModule :: Lens' ScoperResult (Module 'Scoped 'ModuleTop)
mainModule = resultModule

mainModuleSope :: ScoperResult -> Scope
mainModuleSope r =
  r
    ^?! resultScope
      . at (r ^. mainModule . modulePath . Scoped.nameConcrete)
      . _Just
