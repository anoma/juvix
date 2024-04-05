module Juvix.Compiler.Casm.Pretty
  ( module Juvix.Compiler.Casm.Pretty,
    module Juvix.Compiler.Casm.Pretty.Base,
    module Juvix.Compiler.Casm.Pretty.Options,
  )
where

import Juvix.Compiler.Casm.Language
import Juvix.Compiler.Casm.Pretty.Base
import Juvix.Compiler.Casm.Pretty.Options
import Juvix.Data.PPOutput
import Prettyprinter.Render.Terminal qualified as Ansi

ppOutDefault :: (PrettyCode c) => c -> AnsiText
ppOutDefault = mkAnsiText . PPOutput . doc defaultOptions

ppOut :: (PrettyCode c) => Options -> c -> AnsiText
ppOut o = mkAnsiText . PPOutput . doc o

ppTrace' :: (PrettyCode c) => Options -> c -> Text
ppTrace' opts = Ansi.renderStrict . reAnnotateS stylize . layoutPretty defaultLayoutOptions . doc opts

ppTrace :: (PrettyCode c) => c -> Text
ppTrace = ppTrace' defaultOptions

ppPrint :: (PrettyCode c) => c -> Text
ppPrint = show . ppOutDefault

ppProgram :: Code -> AnsiText
ppProgram = mkAnsiText . doc defaultOptions
