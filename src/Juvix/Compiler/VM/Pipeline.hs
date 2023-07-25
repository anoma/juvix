module Juvix.Compiler.VM.Pipeline
  ( module Juvix.Compiler.VM.Pipeline,
    module Juvix.Compiler.VM.Options,
  )
where

import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.VM.Language
import Juvix.Compiler.VM.Options
import Juvix.Compiler.VM.Serialization.ToVampIR

toVampIR' :: Members '[Error LabelError, Reader Options] r => Code -> Sem r ByteString
toVampIR' code = ask >>= flip serialize code

toVampIR :: Members '[Error JuvixError, Reader EntryPoint] r => Code -> Sem r ByteString
toVampIR = mapReader fromEntryPoint . mapError (JuvixError @LabelError) . toVampIR'
