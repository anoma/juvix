module Juvix.Compiler.Internal.Translation.FromConcrete.Data.Context
  ( module Juvix.Compiler.Internal.Translation.FromConcrete.Data.Context,
    module Juvix.Compiler.Internal.Data.InfoTable,
  )
where

import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Data.Context qualified as Concrete
import Juvix.Compiler.Internal.Data.InfoTable
import Juvix.Compiler.Internal.Language
import Juvix.Prelude

data InternalResult = InternalResult
  { _resultScoper :: Concrete.ScoperResult,
    _resultInternalModule :: InternalModule,
    _resultModule :: Module
  }

makeLenses ''InternalResult
