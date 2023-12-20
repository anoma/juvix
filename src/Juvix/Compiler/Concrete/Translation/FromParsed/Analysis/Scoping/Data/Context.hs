module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Data.Context where

import Juvix.Compiler.Concrete.Data.Scope
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromSource.Data.Context qualified as Parsed
import Juvix.Compiler.Concrete.Translation.FromSource.Data.ParserState qualified as Parsed
import Juvix.Compiler.Store.Scoped.Language
import Juvix.Prelude

data ScoperResult = ScoperResult
  { _resultParserResult :: Parsed.ParserResult,
    _resultModule :: Module 'Scoped 'ModuleTop,
    _resultScopedModule :: ScopedModule,
    _resultExports :: HashSet NameId,
    _resultScoperState :: ScoperState,
    _resultScope :: Scope
  }

makeLenses ''ScoperResult

mainModule :: Lens' ScoperResult (Module 'Scoped 'ModuleTop)
mainModule = resultModule

getScoperResultComments :: ScoperResult -> Comments
getScoperResultComments sr = mkComments $ sr ^. resultParserResult . Parsed.resultParserState . Parsed.parserStateComments
