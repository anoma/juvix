module MiniJuvix.Syntax.Concrete.Scoped.Pretty
  ( module MiniJuvix.Syntax.Concrete.Scoped.Pretty,
    module MiniJuvix.Syntax.Concrete.Scoped.Pretty.Base,
    module MiniJuvix.Syntax.Concrete.Scoped.Pretty.Options,
  )
where

import MiniJuvix.Prelude
import MiniJuvix.Prelude.Pretty
import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Ann
import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Ansi qualified as Ansi
import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Base
import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Options

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
