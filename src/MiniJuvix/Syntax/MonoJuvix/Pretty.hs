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

newtype PPOutput = PPOutput (SimpleDocStream Ann)

ppOut :: PrettyCode c => c -> PPOutput
ppOut = PPOutput . docStream defaultOptions

ppOut' :: PrettyCode c => Options -> c -> PPOutput
ppOut' o = PPOutput . docStream o

instance HasAnsiBackend PPOutput where
  toAnsi (PPOutput o) = reAnnotateS Ansi.stylize o

instance HasTextBackend PPOutput where
  toText (PPOutput o) = unAnnotateS o
