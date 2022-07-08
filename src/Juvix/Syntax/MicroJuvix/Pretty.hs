module Juvix.Syntax.MicroJuvix.Pretty
  ( module Juvix.Syntax.MicroJuvix.Pretty,
    module Juvix.Syntax.MicroJuvix.Pretty.Options,
  )
where

import Juvix.Prelude
import Juvix.Prelude.Pretty
import Juvix.Syntax.MicroJuvix.Pretty.Ann
import Juvix.Syntax.MicroJuvix.Pretty.Ansi qualified as Ansi
import Juvix.Syntax.MicroJuvix.Pretty.Base
import Juvix.Syntax.MicroJuvix.Pretty.Options
import Prettyprinter.Render.Terminal qualified as Ansi

newtype PPOutput = PPOutput (Doc Ann)

ppOutDefault :: PrettyCode c => c -> AnsiText
ppOutDefault = AnsiText . PPOutput . doc defaultOptions

ppOut :: PrettyCode c => Options -> c -> AnsiText
ppOut o = AnsiText . PPOutput . doc o

ppSimple :: PrettyCode c => c -> Text
ppSimple = Ansi.renderStrict . reAnnotateS Ansi.stylize . layoutPretty defaultLayoutOptions . doc defaultOptions

instance HasAnsiBackend PPOutput where
  toAnsiStream (PPOutput o) = reAnnotateS Ansi.stylize (layoutPretty defaultLayoutOptions o)
  toAnsiDoc (PPOutput o) = reAnnotate Ansi.stylize o

instance HasTextBackend PPOutput where
  toTextDoc (PPOutput o) = unAnnotate o
  toTextStream (PPOutput o) = unAnnotateS (layoutPretty defaultLayoutOptions o)
