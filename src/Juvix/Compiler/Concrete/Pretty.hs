module Juvix.Compiler.Concrete.Pretty
  ( module Juvix.Compiler.Concrete.Pretty,
    module Juvix.Compiler.Concrete.Pretty.Base,
    module Juvix.Compiler.Concrete.Pretty.Options,
  )
where

import Juvix.Compiler.Concrete.Pretty.Base
import Juvix.Compiler.Concrete.Pretty.Options
import Juvix.Data.PPOutput
import Juvix.Prelude
import Juvix.Prelude.Pretty

ppOutDefault :: PrettyCode c => c -> AnsiText
ppOutDefault = AnsiText . PPOutput . doc defaultOptions

ppOut :: PrettyCode c => Options -> c -> AnsiText
ppOut o = AnsiText . PPOutput . doc o
