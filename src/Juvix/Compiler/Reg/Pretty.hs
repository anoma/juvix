module Juvix.Compiler.Reg.Pretty
  ( module Juvix.Compiler.Reg.Pretty,
    module Juvix.Compiler.Reg.Pretty.Base,
    module Juvix.Compiler.Reg.Pretty.Options,
  )
where

import Juvix.Compiler.Reg.Data.InfoTable
import Juvix.Compiler.Reg.Pretty.Base
import Juvix.Compiler.Reg.Pretty.Options
import Juvix.Data.PPOutput
import Juvix.Prelude
import Prettyprinter.Render.Terminal qualified as Ansi

ppOutDefault :: (PrettyCode c) => InfoTable -> c -> AnsiText
ppOutDefault tab = mkAnsiText . PPOutput . doc (defaultOptions tab)

ppOut :: (PrettyCode c) => Options -> c -> AnsiText
ppOut o = mkAnsiText . PPOutput . doc o

ppTrace' :: (PrettyCode c) => Options -> c -> Text
ppTrace' opts = Ansi.renderStrict . reAnnotateS stylize . layoutPretty defaultLayoutOptions . doc opts

ppTrace :: (PrettyCode c) => InfoTable -> c -> Text
ppTrace tab = ppTrace' (defaultOptions tab)

ppPrint :: (PrettyCode c) => InfoTable -> c -> Text
ppPrint tab = show . ppOutDefault tab
