module MiniJuvix.Syntax.MiniHaskell.Pretty
  ( module MiniJuvix.Syntax.MiniHaskell.Pretty,
    module MiniJuvix.Syntax.MiniHaskell.Pretty.Options,
  )
where

import MiniJuvix.Prelude
import MiniJuvix.Prelude.Pretty
import MiniJuvix.Syntax.MiniHaskell.Pretty.Ansi qualified as Ansi
import MiniJuvix.Syntax.MiniHaskell.Pretty.Base
import MiniJuvix.Syntax.MiniHaskell.Pretty.Options

newtype PPOutput = PPOutput (SimpleDocStream Ann)

ppOutDefault :: PrettyCode c => c -> PPOutput
ppOutDefault = PPOutput . docStream defaultOptions

ppOut :: PrettyCode c => Options -> c -> PPOutput
ppOut o = PPOutput . docStream o

instance HasAnsiBackend PPOutput where
  toAnsi (PPOutput o) = reAnnotateS Ansi.stylize o

instance HasTextBackend PPOutput where
  toText (PPOutput o) = unAnnotateS o
