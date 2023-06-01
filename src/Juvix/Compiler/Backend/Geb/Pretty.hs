module Juvix.Compiler.Backend.Geb.Pretty
  ( module Juvix.Compiler.Backend.Geb.Pretty,
    module Juvix.Compiler.Backend.Geb.Pretty.Base,
    module Juvix.Compiler.Backend.Geb.Pretty.Options,
    module Juvix.Data.PPOutput,
  )
where

import Juvix.Compiler.Backend.Geb.Language
import Juvix.Compiler.Backend.Geb.Pretty.Base
import Juvix.Compiler.Backend.Geb.Pretty.Options
import Juvix.Data.PPOutput
import Prettyprinter.Render.Terminal qualified as Ansi

ppOutDefault :: (HasAtomicity c, PrettyCode c) => c -> AnsiText
ppOutDefault = mkAnsiText . PPOutput . doc defaultOptions

ppOut :: (CanonicalProjection a Options, HasAtomicity c, PrettyCode c) => a -> c -> AnsiText
ppOut o = mkAnsiText . PPOutput . doc (project o)

ppTrace' :: (CanonicalProjection a Options, HasAtomicity c, PrettyCode c) => a -> c -> Text
ppTrace' opts = Ansi.renderStrict . reAnnotateS stylize . layoutPretty defaultLayoutOptions . doc (project opts)

ppTrace :: (HasAtomicity c, PrettyCode c) => c -> Text
ppTrace = ppTrace' traceOptions

ppPrint :: (HasAtomicity c, PrettyCode c) => c -> Text
ppPrint = show . ppOutDefault

ppPrintLisp :: Text -> Text -> Morphism -> Object -> Text
ppPrintLisp packageName entryName morph =
  show . mkAnsiText . PPOutput . docLisp defaultOptions packageName entryName morph
