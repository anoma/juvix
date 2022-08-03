module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Data.Context where

import Juvix.Compiler.Concrete.Data.InfoTable
import Juvix.Compiler.Concrete.Data.ParsedInfoTable qualified as Parsed
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromSource.Data.Context qualified as Parsed
import Juvix.Prelude

data ScoperResult = ScoperResult
  { _resultParserResult :: Parsed.ParserResult,
    _resultParserTable :: Parsed.InfoTable,
    _resultScoperTable :: InfoTable,
    _resultModules :: NonEmpty (Module 'Scoped 'ModuleTop),
    _resultExports :: HashSet NameId
  }

makeLenses ''ScoperResult

mainModule :: Lens' ScoperResult (Module 'Scoped 'ModuleTop)
mainModule = resultModules . _head
