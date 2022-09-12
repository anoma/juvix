module Juvix.Compiler.Core.Pretty
  ( module Juvix.Compiler.Core.Pretty,
    module Juvix.Compiler.Core.Pretty.Base,
    module Juvix.Compiler.Core.Pretty.Options,
    module Juvix.Data.PPOutput,
  )
where

import Juvix.Compiler.Core.Pretty.Base
import Juvix.Compiler.Core.Pretty.Options
import Juvix.Data.PPOutput
import Juvix.Prelude
import Prettyprinter.Render.Terminal qualified as Ansi

ppOutDefault :: PrettyCode c => c -> AnsiText
ppOutDefault = AnsiText . PPOutput . doc defaultOptions

ppOut :: (CanonicalProjection a Options, PrettyCode c) => a -> c -> AnsiText
ppOut o = AnsiText . PPOutput . doc (project o)

ppTrace' :: (CanonicalProjection a Options, PrettyCode c) => a -> c -> Text
ppTrace' opts = Ansi.renderStrict . reAnnotateS stylize . layoutPretty defaultLayoutOptions . doc (project opts)

ppTrace :: PrettyCode c => c -> Text
ppTrace = ppTrace' defaultOptions

ppPrint :: PrettyCode c => c -> Text
ppPrint = show . ppOutDefault
