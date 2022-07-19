module Juvix.Syntax.MicroJuvix.MicroJuvixArityResult
  ( module Juvix.Syntax.MicroJuvix.MicroJuvixArityResult,
  )
where

import Juvix.Pipeline.EntryPoint qualified as E
import Juvix.Prelude
import Juvix.Syntax.MicroJuvix.Language
import Juvix.Syntax.MicroJuvix.MicroJuvixResult qualified as M

data MicroJuvixArityResult = MicroJuvixArityResult
  { _resultMicroJuvixResult :: M.MicroJuvixResult,
    _resultModules :: NonEmpty Module
  }

makeLenses ''MicroJuvixArityResult

mainModule :: Lens' MicroJuvixArityResult Module
mainModule = resultModules . _head

microJuvixArityResultEntryPoint :: Lens' MicroJuvixArityResult E.EntryPoint
microJuvixArityResultEntryPoint = resultMicroJuvixResult . M.microJuvixResultEntryPoint
