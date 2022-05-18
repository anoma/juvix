module MiniJuvix.Syntax.MonoJuvix.Pretty
  ( module MiniJuvix.Syntax.MonoJuvix.Pretty,
    module MiniJuvix.Syntax.MonoJuvix.Pretty.Options,
  )
where

import MiniJuvix.Prelude
import MiniJuvix.Prelude.Pretty
import MiniJuvix.Syntax.MonoJuvix.Pretty.Ann
import MiniJuvix.Syntax.MonoJuvix.Pretty.Ansi qualified as Ansi
import MiniJuvix.Syntax.MonoJuvix.Pretty.Base
import MiniJuvix.Syntax.MonoJuvix.Pretty.Options

newtype PPOutput = PPOutput (Doc Ann)

ppOutDefault :: PrettyCode c => c -> AnsiText
ppOutDefault = AnsiText . PPOutput . doc defaultOptions

ppOut :: PrettyCode c => Options -> c -> AnsiText
ppOut o = AnsiText . PPOutput . doc o

instance HasAnsiBackend PPOutput where
  toAnsiStream (PPOutput o) = reAnnotateS Ansi.stylize (layoutPretty defaultLayoutOptions o)
  toAnsiDoc (PPOutput o) = reAnnotate Ansi.stylize o

instance HasTextBackend PPOutput where
  toTextDoc (PPOutput o) = unAnnotate o
  toTextStream (PPOutput o) = unAnnotateS (layoutPretty defaultLayoutOptions o)
