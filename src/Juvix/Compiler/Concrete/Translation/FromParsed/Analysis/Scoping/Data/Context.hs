module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Data.Context where

import Juvix.Compiler.Concrete.Data.InfoTable
import Juvix.Compiler.Concrete.Data.ParsedInfoTable qualified as Parsed
import Juvix.Compiler.Concrete.Data.Scope
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromSource.Data.Context qualified as Parsed
import Juvix.Data.Comment
import Juvix.Prelude

data ScoperResult = ScoperResult
  { _resultParserResult :: Parsed.ParserResult,
    _resultParserTable :: Parsed.InfoTable,
    _resultScoperTable :: InfoTable,
    _resultModules :: NonEmpty (Module 'Scoped 'ModuleTop),
    _resultExports :: HashSet NameId,
    _resultScope :: HashMap TopModulePath Scope
  }

makeLenses ''ScoperResult

mainModule :: Lens' ScoperResult (Module 'Scoped 'ModuleTop)
mainModule = resultModules . _head1

comments :: Lens' ScoperResult Comments
comments = resultParserResult . Parsed.resultTable . Parsed.infoParsedComments
