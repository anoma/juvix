module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Data.Context
  ( module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Data.Context,
    module Juvix.Compiler.Concrete.Data.InfoTable,
  )
where

import Juvix.Compiler.Concrete.Data.InfoTable
import Juvix.Compiler.Concrete.Data.ParsedInfoTable qualified as Parsed
import Juvix.Compiler.Concrete.Data.Scope
import Juvix.Compiler.Concrete.Data.ScopedName qualified as Scoped
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromSource.Data.Context qualified as Parsed
import Juvix.Prelude

data ScoperResult = ScoperResult
  { _resultParserResult :: Parsed.ParserResult,
    _resultScoperTable :: InfoTable,
    _resultModules :: NonEmpty (Module 'Scoped 'ModuleTop),
    _resultExports :: HashSet NameId,
    _resultScope :: HashMap TopModulePath Scope
  }

makeLenses ''ScoperResult

mainModule :: Lens' ScoperResult (Module 'Scoped 'ModuleTop)
mainModule = resultModules . _head1

mainModuleSope :: ScoperResult -> Scope
mainModuleSope r =
  r
    ^?! resultScope
      . at (r ^. mainModule . modulePath . Scoped.nameConcrete)
      . _Just

comments :: Lens' ScoperResult Comments
comments = resultParserResult . Parsed.resultTable . Parsed.infoParsedComments
