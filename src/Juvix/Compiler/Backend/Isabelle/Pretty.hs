module Juvix.Compiler.Backend.Isabelle.Pretty where

import Juvix.Compiler.Backend.Isabelle.Pretty.Base
import Juvix.Compiler.Backend.Isabelle.Pretty.Options
import Juvix.Data.PPOutput
import Juvix.Prelude
import Prettyprinter.Render.Terminal qualified as Ansi

ppOutDefault :: (PrettyCode c) => [Comment] -> c -> AnsiText
ppOutDefault comments = mkAnsiText . PPOutput . doc (defaultOptions comments)

ppOut :: (CanonicalProjection a Options, PrettyCode c) => a -> c -> AnsiText
ppOut o = mkAnsiText . PPOutput . doc (project o)

ppTrace' :: (CanonicalProjection a Options, PrettyCode c) => a -> c -> Text
ppTrace' opts = Ansi.renderStrict . reAnnotateS stylize . layoutPretty defaultLayoutOptions . doc (project opts)

ppTrace :: (PrettyCode c) => c -> Text
ppTrace = ppTrace' traceOptions

ppPrint :: (PrettyCode c) => [Comment] -> c -> Text
ppPrint comments = show . ppOutDefault comments
