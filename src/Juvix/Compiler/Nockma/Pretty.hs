module Juvix.Compiler.Nockma.Pretty
  ( module Juvix.Compiler.Nockma.Pretty,
    module Juvix.Compiler.Nockma.Language,
    module Juvix.Compiler.Nockma.Pretty.Base,
    module Juvix.Compiler.Nockma.Pretty.Options,
    module Juvix.Data.PPOutput,
  )
where

import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Pretty.Base
import Juvix.Compiler.Nockma.Pretty.Options
import Juvix.Data.PPOutput
import Juvix.Prelude
import Prettyprinter.Render.Terminal qualified as Ansi

ppOutDefault :: (PrettyCode c) => c -> AnsiText
ppOutDefault = mkAnsiText . PPOutput . doc defaultOptions

ppOut :: (CanonicalProjection a Options, PrettyCode c) => a -> c -> AnsiText
ppOut o = mkAnsiText . PPOutput . doc (project o)

ppTrace :: (PrettyCode c) => c -> Text
ppTrace =
  Ansi.renderStrict . reAnnotateS stylize . layoutPretty defaultLayoutOptions . doc defaultOptions

ppSerialize :: (PrettyCode c) => c -> Text
ppSerialize = ppPrintOpts serializeOptions

ppPrint :: (PrettyCode c) => c -> Text
ppPrint = toPlainText . ppOut defaultOptions

ppTest :: (PrettyCode c) => c -> Text
ppTest = toPlainText . ppOut testOptions

ppPrintOpts :: (PrettyCode c) => Options -> c -> Text
ppPrintOpts opts = renderStrict . toTextStream . ppOut opts
