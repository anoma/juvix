module Juvix.Compiler.Concrete.Print
  ( module Juvix.Compiler.Concrete.Print,
    module Juvix.Compiler.Concrete.Print.Base,
    module Juvix.Data.Effect.ExactPrint,
  )
where

import Juvix.Compiler.Concrete.Pretty.Options
import Juvix.Compiler.Concrete.Print.Base
import Juvix.Data.Effect.ExactPrint
import Juvix.Data.PPOutput
import Juvix.Prelude

ppOutDefault :: (HasLoc c, PrettyPrint c) => Comments -> c -> AnsiText
ppOutDefault cs = AnsiText . PPOutput . doc defaultOptions cs

ppOut :: (CanonicalProjection a Options, PrettyPrint c, HasLoc c) => a -> Comments -> c -> AnsiText
ppOut o cs = AnsiText . PPOutput . doc (project o) cs

ppOutNoComments :: (CanonicalProjection a Options, PrettyPrint c, HasLoc c) => a -> c -> AnsiText
ppOutNoComments o = AnsiText . PPOutput . doc (project o) emptyComments
