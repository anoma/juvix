module Juvix.Compiler.Concrete.Print
  ( module Juvix.Compiler.Concrete.Print,
    module Juvix.Compiler.Concrete.Print.Base,
    module Juvix.Data.Effect.ExactPrint,
  )
where

import Juvix.Compiler.Concrete.Language.Base
import Juvix.Compiler.Concrete.Pretty.Options
import Juvix.Compiler.Concrete.Print.Base
import Juvix.Data.Effect.ExactPrint
import Juvix.Data.PPOutput
import Juvix.Prelude

ppOutDefaultNoComments :: (PrettyPrint c) => c -> AnsiText
ppOutDefaultNoComments = mkAnsiText . PPOutput . docNoComments defaultOptions

ppOutDefault :: (HasLoc c, PrettyPrint c) => Comments -> c -> AnsiText
ppOutDefault cs = mkAnsiText . PPOutput . docDefault (Just cs)

ppOut :: (CanonicalProjection a Options, PrettyPrint c, HasLoc c) => a -> Comments -> c -> AnsiText
ppOut o cs = mkAnsiText . PPOutput . doc (project o) (Just cs)

ppOutNoComments :: (CanonicalProjection a Options, PrettyPrint c) => a -> c -> AnsiText
ppOutNoComments o = mkAnsiText . PPOutput . docNoLoc (project o)

ppOutJudoc :: (SingI s) => Judoc s -> AnsiText
ppOutJudoc = ppOutNoComments defaultOptions {_optJudoc = True}
