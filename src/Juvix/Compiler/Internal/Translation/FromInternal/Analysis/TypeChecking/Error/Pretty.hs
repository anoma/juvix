module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error.Pretty
  ( module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error.Pretty,
    module Juvix.Data.CodeAnn,
  )
where

import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Pretty.Base qualified as Internal
import Juvix.Data.CodeAnn
import Juvix.Prelude

ppCode :: (Internal.PrettyCode c) => Internal.Options -> c -> Doc Ann
ppCode opts = runPP opts . Internal.ppCode

ppAtom :: (Internal.PrettyCode c, HasAtomicity c) => Internal.Options -> c -> Doc Ann
ppAtom opts = runPP opts . Internal.ppCodeAtom

runPP :: Internal.Options -> Sem '[Reader Internal.Options] (Doc Internal.Ann) -> Doc Ann
runPP opts = highlight_ . run . runReader opts

highlight_ :: Doc Ann -> Doc Ann
highlight_ = annotate AnnCode

ppApp :: Internal.Options -> (Expression, [ApplicationArg]) -> Doc Ann
ppApp opts (fun, args) =
  hsep (ppAtom opts fun : map (ppArg opts) args)

ppArg :: Internal.Options -> ApplicationArg -> Doc Ann
ppArg opts arg = case arg ^. appArgIsImplicit of
  ImplicitInstance -> doubleBraces (ppCode opts e)
  Implicit -> braces (ppCode opts e)
  Explicit -> ppAtom opts e
  where
    e = arg ^. appArg
