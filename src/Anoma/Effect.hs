module Anoma.Effect
  ( module Anoma.Effect,
    module Anoma.Effect.Base,
  )
where

import Anoma.Effect.Base
import Juvix.Compiler.Nockma.Language qualified as Nockma
import Juvix.Prelude

data RunNockmaInput = RunNockmaInput
  { _runNockmaProgram :: AnomaResult,
    _runNockmaInput :: [Nockma.Term Natural]
  }

makeLenses ''RunNockmaInput

runNockma :: (Members '[Anoma] r) => Nockma.Term Natural -> [Nockma.Term Natural] -> Sem r (Nockma.Term Natural)
runNockma prog inputs = undefined
