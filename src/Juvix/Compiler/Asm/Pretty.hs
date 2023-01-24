module Juvix.Compiler.Asm.Pretty
  ( module Juvix.Compiler.Asm.Pretty,
    module Juvix.Compiler.Asm.Pretty.Base,
    module Juvix.Compiler.Asm.Pretty.Options,
  )
where

import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Asm.Pretty.Base
import Juvix.Compiler.Asm.Pretty.Options
import Juvix.Data.PPOutput
import Juvix.Prelude
import Prettyprinter.Render.Terminal qualified as Ansi

ppOutDefault :: (PrettyCode c) => InfoTable -> c -> AnsiText
ppOutDefault tab = AnsiText . PPOutput . doc (defaultOptions tab)

ppOut :: (PrettyCode c) => Options -> c -> AnsiText
ppOut o = AnsiText . PPOutput . doc o

ppTrace' :: (PrettyCode c) => Options -> c -> Text
ppTrace' opts = Ansi.renderStrict . reAnnotateS stylize . layoutPretty defaultLayoutOptions . doc opts

ppTrace :: (PrettyCode c) => InfoTable -> c -> Text
ppTrace tab = ppTrace' (defaultOptions tab)

ppPrint :: (PrettyCode c) => InfoTable -> c -> Text
ppPrint tab = show . ppOutDefault tab
