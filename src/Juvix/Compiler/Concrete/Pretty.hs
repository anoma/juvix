module Juvix.Compiler.Concrete.Pretty
  ( module Juvix.Compiler.Concrete.Pretty,
    module Juvix.Compiler.Concrete.Pretty.Options,
    module Juvix.Compiler.Concrete.Print,
  )
where

import Juvix.Compiler.Concrete.Pretty.Options
import Juvix.Compiler.Concrete.Print (PrettyPrint, doc, docNoComments)
import Juvix.Compiler.Concrete.Print qualified as Print
import Juvix.Data.PPOutput
import Juvix.Prelude

ppOutDefault :: (PrettyPrint c, HasLoc c) => c -> AnsiText
ppOutDefault = Print.ppOutNoComments defaultOptions

ppOut :: (HasLoc c, CanonicalProjection a Options, PrettyPrint c) => a -> c -> AnsiText
ppOut = Print.ppOutNoComments

ppTrace :: (HasLoc c, PrettyPrint c) => c -> Text
ppTrace = toAnsiText True . ppOut traceOptions
