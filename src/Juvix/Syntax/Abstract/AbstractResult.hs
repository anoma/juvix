module Juvix.Syntax.Abstract.AbstractResult
  ( module Juvix.Syntax.Abstract.AbstractResult,
    module Juvix.Syntax.Abstract.InfoTable,
  )
where

import Juvix.Pipeline.EntryPoint qualified as E
import Juvix.Prelude
import Juvix.Syntax.Abstract.InfoTable
import Juvix.Syntax.Abstract.Language
import Juvix.Syntax.Concrete.Parser.ParserResult
import Juvix.Syntax.Concrete.Scoped.Scoper.ScoperResult

data AbstractResult = AbstractResult
  { _resultScoper :: ScoperResult,
    _resultTable :: InfoTable,
    _resultModules :: NonEmpty TopModule
  }

makeLenses ''AbstractResult

abstractResultEntryPoint :: Lens' AbstractResult E.EntryPoint
abstractResultEntryPoint = resultScoper . resultParserResult . resultEntry
