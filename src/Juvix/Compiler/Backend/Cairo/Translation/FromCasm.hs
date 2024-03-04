module Juvix.Compiler.Backend.Cairo.Translation.FromCasm where

import Juvix.Compiler.Backend.Cairo.Language
import Juvix.Compiler.Casm.Data.LabelInfo qualified as Casm
import Juvix.Compiler.Casm.Language qualified as Casm

fromCasm :: Casm.LabelInfo -> Casm.Code -> [Element]
fromCasm labInfo instrs = undefined
