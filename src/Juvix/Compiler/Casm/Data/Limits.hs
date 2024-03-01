module Juvix.Compiler.Casm.Data.Limits where

import Juvix.Compiler.Backend
import Juvix.Compiler.Casm.Language

casmMaxFunctionArgs :: Int
casmMaxFunctionArgs = getLimits TargetCairo False ^. limitsMaxFunctionArgs

casmClosureAddrOffset :: Offset
casmClosureAddrOffset = 0

casmClosureStoredArgsOffset :: Offset
casmClosureStoredArgsOffset = 1

casmClosureArgsNumOffset :: Offset
casmClosureArgsNumOffset = 2
