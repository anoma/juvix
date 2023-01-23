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

ppOutDefault :: (PrettyCode c) => c -> AnsiText
ppOutDefault = AnsiText . PPOutput . doc defaultOptions

ppOut :: (CanonicalProjection a Options, PrettyCode c) => a -> c -> AnsiText
ppOut o = AnsiText . PPOutput . doc (project o)
