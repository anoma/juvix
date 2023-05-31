module Juvix.Compiler.Abstract.Pretty
  ( module Juvix.Compiler.Abstract.Pretty,
    module Juvix.Compiler.Abstract.Pretty.Options,
  )
where

import Juvix.Compiler.Abstract.Pretty.Base
import Juvix.Compiler.Abstract.Pretty.Options
import Juvix.Data.PPOutput
import Juvix.Prelude

ppOutDefault :: (PrettyCode c) => c -> AnsiText
ppOutDefault = mkAnsiText . PPOutput . doc defaultOptions

ppOut :: (CanonicalProjection a Options, PrettyCode c) => a -> c -> AnsiText
ppOut o = mkAnsiText . PPOutput . doc (project o)

ppTrace :: (PrettyCode c) => c -> Text
ppTrace = toAnsiText True . ppOutDefault
