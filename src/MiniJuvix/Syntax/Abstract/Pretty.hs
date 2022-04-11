module MiniJuvix.Syntax.Abstract.Pretty where


import MiniJuvix.Prelude.Pretty
import qualified MiniJuvix.Syntax.Abstract.Pretty.Ansi as Ansi
import MiniJuvix.Syntax.Abstract.Pretty.Base
import MiniJuvix.Prelude


newtype PPOutput = PPOutput (SimpleDocStream Ann)

ppOut :: PrettyCode c => c -> PPOutput
ppOut = PPOutput . docStream defaultOptions

ppOut' :: PrettyCode c => Options -> c -> PPOutput
ppOut' o = PPOutput . docStream o

instance HasAnsiBackend PPOutput where
  toAnsi (PPOutput o) = reAnnotateS Ansi.stylize o

instance HasTextBackend PPOutput where
  toText (PPOutput o) = unAnnotateS o
