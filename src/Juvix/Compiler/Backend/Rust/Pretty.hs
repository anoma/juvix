module Juvix.Compiler.Backend.Rust.Pretty
  ( module Juvix.Compiler.Backend.Rust.Pretty,
    module Juvix.Compiler.Backend.Rust.Pretty.Base,
    module Juvix.Compiler.Backend.Rust.Pretty.Options,
    module Juvix.Data.PPOutput,
  )
where

import Juvix.Compiler.Backend.Rust.Pretty.Base
import Juvix.Compiler.Backend.Rust.Pretty.Options
import Juvix.Data.PPOutput
import Juvix.Prelude
import Prettyprinter.Render.Terminal qualified as Ansi

ppOutDefault :: (PrettyCode c) => c -> AnsiText
ppOutDefault = mkAnsiText . PPOutput . doc defaultOptions

ppOut :: (CanonicalProjection a Options, PrettyCode c) => a -> c -> AnsiText
ppOut o = mkAnsiText . PPOutput . doc (project o)

ppTrace' :: (CanonicalProjection a Options, PrettyCode c) => a -> c -> Text
ppTrace' opts = Ansi.renderStrict . reAnnotateS stylize . layoutPretty defaultLayoutOptions . doc (project opts)

ppTrace :: (PrettyCode c) => c -> Text
ppTrace = ppTrace' traceOptions

ppPrint :: (PrettyCode c) => c -> Text
ppPrint = show . ppOutDefault
