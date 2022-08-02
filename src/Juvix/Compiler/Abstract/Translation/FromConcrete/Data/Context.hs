module Juvix.Compiler.Abstract.Translation.FromConcrete.Data.Context
  ( module Juvix.Compiler.Abstract.Translation.FromConcrete.Data.Context,
    module Juvix.Compiler.Abstract.Data.InfoTable,
  )
where

import Juvix.Compiler.Abstract.Data.InfoTable
import Juvix.Compiler.Abstract.Language
import Juvix.Compiler.Concrete.Translation.FromParsed qualified as Concrete
import Juvix.Compiler.Concrete.Translation.FromSource qualified as Concrete
import Juvix.Compiler.Pipeline.EntryPoint qualified as E
import Juvix.Prelude

data AbstractResult = AbstractResult
  { _resultScoper :: Concrete.ScoperResult,
    _resultTable :: InfoTable,
    _resultModules :: NonEmpty TopModule,
    _resultExports :: HashSet NameId
  }

makeLenses ''AbstractResult

abstractResultEntryPoint :: Lens' AbstractResult E.EntryPoint
abstractResultEntryPoint = resultScoper . Concrete.resultParserResult . Concrete.resultEntry
