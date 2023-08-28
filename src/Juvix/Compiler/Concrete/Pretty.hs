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

ppOutDefault :: (PrettyPrint c) => c -> AnsiText
ppOutDefault = Print.ppOutNoComments defaultOptions

ppOut :: (CanonicalProjection a Options, PrettyPrint c) => a -> c -> AnsiText
ppOut = Print.ppOutNoComments

ppTrace :: (PrettyPrint c) => c -> Text
ppTrace = toAnsiText True . ppOut traceOptions
