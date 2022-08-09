module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Data.Context where

import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Internal.Translation.FromAbstract.Data.Context qualified as M
import Juvix.Compiler.Pipeline.EntryPoint qualified as E
import Juvix.Prelude

data InternalArityResult = InternalArityResult
  { _resultInternalResult :: M.InternalResult,
    _resultModules :: NonEmpty Module
  }

makeLenses ''InternalArityResult

mainModule :: Lens' InternalArityResult Module
mainModule = resultModules . _head

internalArityResultEntryPoint :: Lens' InternalArityResult E.EntryPoint
internalArityResultEntryPoint = resultInternalResult . M.microJuvixResultEntryPoint
