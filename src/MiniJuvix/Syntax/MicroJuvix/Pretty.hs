module MiniJuvix.Syntax.MicroJuvix.Pretty
  ( module MiniJuvix.Syntax.MicroJuvix.Pretty,
    module MiniJuvix.Syntax.MicroJuvix.Pretty.Options,
  )
where

import MiniJuvix.Prelude
import MiniJuvix.Prelude.Pretty
import MiniJuvix.Syntax.MicroJuvix.Pretty.Ann
import MiniJuvix.Syntax.MicroJuvix.Pretty.Ansi qualified as Ansi
import MiniJuvix.Syntax.MicroJuvix.Pretty.Base
import MiniJuvix.Syntax.MicroJuvix.Pretty.Options

newtype PPOutput = PPOutput (SimpleDocStream Ann)

ppOutDefault :: PrettyCode c => c -> PPOutput
ppOutDefault = PPOutput . docStream defaultOptions

ppOut :: PrettyCode c => Options -> c -> PPOutput
ppOut o = PPOutput . docStream o

instance HasAnsiBackend PPOutput where
  toAnsi (PPOutput o) = reAnnotateS Ansi.stylize o

instance HasTextBackend PPOutput where
  toText (PPOutput o) = unAnnotateS o
