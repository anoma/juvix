module MiniJuvix.Syntax.Abstract.Pretty
  ( module MiniJuvix.Syntax.Abstract.Pretty,
    module MiniJuvix.Syntax.Abstract.Pretty.Options,
  )
where

import MiniJuvix.Prelude
import MiniJuvix.Prelude.Pretty
import MiniJuvix.Syntax.Abstract.Pretty.Ansi qualified as Ansi
import MiniJuvix.Syntax.Abstract.Pretty.Base
import MiniJuvix.Syntax.Abstract.Pretty.Options

newtype PPOutput = PPOutput (SimpleDocStream Ann)

ppOutDefault :: PrettyCode c => c -> PPOutput
ppOutDefault = PPOutput . docStream defaultOptions

ppOut :: PrettyCode c => Options -> c -> PPOutput
ppOut o = PPOutput . docStream o

instance HasAnsiBackend PPOutput where
  toAnsi (PPOutput o) = reAnnotateS Ansi.stylize o

instance HasTextBackend PPOutput where
  toText (PPOutput o) = unAnnotateS o
