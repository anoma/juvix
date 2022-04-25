module MiniJuvix.Syntax.Concrete.Scoped.Pretty
  ( module MiniJuvix.Syntax.Concrete.Scoped.Pretty,
    module MiniJuvix.Syntax.Concrete.Scoped.Pretty.Options,
  )
where

import MiniJuvix.Prelude
import MiniJuvix.Prelude.Pretty
import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Ann
import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Ansi qualified as Ansi
import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Base
import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Options

newtype PPOutput = PPOutput (SimpleDocStream Ann)

ppOutDefault :: PrettyCode c => c -> PPOutput
ppOutDefault = PPOutput . docStream defaultOptions

ppOut :: PrettyCode c => Options -> c -> PPOutput
ppOut o = PPOutput . docStream o

instance HasAnsiBackend PPOutput where
  toAnsi (PPOutput o) = reAnnotateS Ansi.stylize o

instance HasTextBackend PPOutput where
  toText (PPOutput o) = unAnnotateS o
