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

ppOut :: PrettyCode c => Options -> c -> AnsiText
ppOut o = AnsiText . PPOutput . doc o

ppTrace :: PrettyCode c => c -> Text
ppTrace = Ansi.renderStrict . reAnnotateS stylize . layoutPretty defaultLayoutOptions . doc defaultOptions

ppPrint :: PrettyCode c => c -> Text
ppPrint = show . ppOutDefault
