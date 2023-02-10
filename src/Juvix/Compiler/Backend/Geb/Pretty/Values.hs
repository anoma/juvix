module Juvix.Compiler.Backend.Geb.Pretty.Values
  ( module Juvix.Compiler.Backend.Geb.Pretty.Values,
    module Juvix.Compiler.Backend.Geb.Pretty.Values.Base,
    module Juvix.Compiler.Backend.Geb.Pretty.Options,
    module Juvix.Data.PPOutput,
  )
where

import Juvix.Compiler.Backend.Geb.Evaluator.Data.Values
import Juvix.Compiler.Backend.Geb.Language
import Juvix.Compiler.Backend.Geb.Pretty.Options
import Juvix.Compiler.Backend.Geb.Pretty.Values.Base
import Juvix.Data.PPOutput
import Prettyprinter.Render.Terminal qualified as Ansi

ppOutDefault :: (HasAtomicity c, PrettyCode c) => c -> AnsiText
ppOutDefault = AnsiText . PPOutput . doc defaultOptions

ppOut :: (CanonicalProjection a Options, HasAtomicity c, PrettyCode c) => a -> c -> AnsiText
ppOut o = AnsiText . PPOutput . doc (project o)

ppTrace' :: (CanonicalProjection a Options, HasAtomicity c, PrettyCode c) => a -> c -> Text
ppTrace' opts = Ansi.renderStrict . reAnnotateS stylize . layoutPretty defaultLayoutOptions . doc (project opts)

ppTrace :: (HasAtomicity c, PrettyCode c) => c -> Text
ppTrace = ppTrace' traceOptions

ppPrint :: (HasAtomicity c, PrettyCode c) => c -> Text
ppPrint = show . ppOutDefault
