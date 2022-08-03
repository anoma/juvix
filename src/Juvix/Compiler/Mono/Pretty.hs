module Juvix.Compiler.Mono.Pretty
  ( module Juvix.Compiler.Mono.Pretty,
    module Juvix.Compiler.Mono.Pretty.Options,
  )
where

import Juvix.Compiler.Mono.Pretty.Ann
import Juvix.Compiler.Mono.Pretty.Ansi qualified as Ansi
import Juvix.Compiler.Mono.Pretty.Base
import Juvix.Compiler.Mono.Pretty.Options
import Juvix.Prelude
import Juvix.Prelude.Pretty

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
