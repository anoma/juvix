module Juvix.Compiler.Mono.Pretty
  ( module Juvix.Compiler.Mono.Pretty,
    module Juvix.Compiler.Mono.Pretty.Options,
  )
where

import Juvix.Compiler.Mono.Pretty.Base
import Juvix.Compiler.Mono.Pretty.Options
import Juvix.Data.PPOutput
import Juvix.Prelude
import Juvix.Prelude.Pretty

ppOutDefault :: PrettyCode c => c -> AnsiText
ppOutDefault = AnsiText . PPOutput . doc defaultOptions

ppOut :: PrettyCode c => Options -> c -> AnsiText
ppOut o = AnsiText . PPOutput . doc o
