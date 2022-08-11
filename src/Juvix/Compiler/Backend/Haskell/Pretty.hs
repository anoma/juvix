module Juvix.Compiler.Backend.Haskell.Pretty
  ( module Juvix.Compiler.Backend.Haskell.Pretty,
    module Juvix.Compiler.Backend.Haskell.Pretty.Options,
  )
where

import Juvix.Compiler.Backend.Haskell.Pretty.Base
import Juvix.Compiler.Backend.Haskell.Pretty.Options
import Juvix.Data.PPOutput
import Juvix.Prelude

ppOutDefault :: PrettyCode c => c -> AnsiText
ppOutDefault = AnsiText . PPOutput . doc defaultOptions

ppOut :: PrettyCode c => Options -> c -> AnsiText
ppOut o = AnsiText . PPOutput . doc o
