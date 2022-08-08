module Juvix.Compiler.Abstract.Pretty
  ( module Juvix.Compiler.Abstract.Pretty,
    module Juvix.Compiler.Abstract.Pretty.Options,
  )
where

import Juvix.Compiler.Abstract.Pretty.Base
import Juvix.Compiler.Abstract.Pretty.Options
import Juvix.Data.PPOutput
import Juvix.Prelude
import Juvix.Prelude.Pretty

ppOutDefault :: PrettyCode c => c -> AnsiText
ppOutDefault = AnsiText . PPOutput . doc defaultOptions

ppOut :: PrettyCode c => Options -> c -> AnsiText
ppOut o = AnsiText . PPOutput . doc o

ppTrace :: PrettyCode c => c -> Text
ppTrace = toAnsiText True . ppOutDefault
