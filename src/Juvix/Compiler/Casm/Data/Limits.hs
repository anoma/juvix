module Juvix.Compiler.Casm.Data.Limits where

import Juvix.Compiler.Backend
import Juvix.Compiler.Casm.Language

casmMaxFunctionArgs :: Int
casmMaxFunctionArgs = getLimits TargetCairo False ^. limitsMaxFunctionArgs + 1

casmClosureAddrOffset :: Offset
casmClosureAddrOffset = 0

casmClosureArgsNumOffset :: Offset
casmClosureArgsNumOffset = 1

casmClosureStoredArgsOffset :: Offset
casmClosureStoredArgsOffset = 2
