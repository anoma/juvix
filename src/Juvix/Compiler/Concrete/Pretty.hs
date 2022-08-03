module Juvix.Compiler.Concrete.Pretty
  ( module Juvix.Compiler.Concrete.Pretty,
    module Juvix.Compiler.Concrete.Pretty.Base,
    module Juvix.Compiler.Concrete.Pretty.Options,
  )
where

import Juvix.Compiler.Concrete.Pretty.Ann
import Juvix.Compiler.Concrete.Pretty.Ansi qualified as Ansi
import Juvix.Compiler.Concrete.Pretty.Base
import Juvix.Compiler.Concrete.Pretty.Options
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
