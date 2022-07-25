module Juvix.Syntax.Abstract.AbstractResult
  ( module Juvix.Syntax.Abstract.AbstractResult,
    module Juvix.Syntax.Abstract.InfoTable,
  )
where

import Juvix.Analysis.Scoping.ScoperResult
import Juvix.Parsing.ParserResult
import Juvix.Pipeline.EntryPoint qualified as E
import Juvix.Prelude
import Juvix.Syntax.Abstract.InfoTable
import Juvix.Syntax.Abstract.Language

data AbstractResult = AbstractResult
  { _resultScoper :: ScoperResult,
    _resultTable :: InfoTable,
    _resultModules :: NonEmpty TopModule,
    _resultExports :: HashSet NameId
  }

makeLenses ''AbstractResult

abstractResultEntryPoint :: Lens' AbstractResult E.EntryPoint
abstractResultEntryPoint = resultScoper . resultParserResult . resultEntry
