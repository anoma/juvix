module Juvix.Compiler.Abstract.Pretty
  ( module Juvix.Compiler.Abstract.Pretty,
    module Juvix.Compiler.Abstract.Pretty.Options,
  )
where

import Juvix.Compiler.Abstract.Pretty.Ansi qualified as Ansi
import Juvix.Compiler.Abstract.Pretty.Base
import Juvix.Compiler.Abstract.Pretty.Options
import Juvix.Prelude
import Juvix.Prelude.Pretty

newtype PPOutput = PPOutput (Doc Ann)

ppOutDefault :: PrettyCode c => c -> AnsiText
ppOutDefault = AnsiText . PPOutput . doc defaultOptions

ppOut :: PrettyCode c => Options -> c -> AnsiText
ppOut o = AnsiText . PPOutput . doc o

ppTrace :: PrettyCode c => c -> Text
ppTrace = toAnsiText True . ppOutDefault

instance HasAnsiBackend PPOutput where
  toAnsiStream (PPOutput o) = reAnnotateS Ansi.stylize (layoutPretty defaultLayoutOptions o)
  toAnsiDoc (PPOutput o) = reAnnotate Ansi.stylize o

instance HasTextBackend PPOutput where
  toTextDoc (PPOutput o) = unAnnotate o
  toTextStream (PPOutput o) = unAnnotateS (layoutPretty defaultLayoutOptions o)
