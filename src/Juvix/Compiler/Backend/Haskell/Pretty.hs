module Juvix.Compiler.Backend.Haskell.Pretty
  ( module Juvix.Compiler.Backend.Haskell.Pretty,
    module Juvix.Compiler.Backend.Haskell.Pretty.Options,
  )
where

import Juvix.Compiler.Backend.Haskell.Pretty.Ansi qualified as Ansi
import Juvix.Compiler.Backend.Haskell.Pretty.Base
import Juvix.Compiler.Backend.Haskell.Pretty.Options
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
