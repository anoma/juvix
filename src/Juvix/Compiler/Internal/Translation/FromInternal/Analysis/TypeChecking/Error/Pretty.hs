module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error.Pretty
  ( module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error.Pretty,
    module Juvix.Data.CodeAnn,
  )
where

import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Pretty.Base qualified as Micro
import Juvix.Data.CodeAnn
import Juvix.Prelude

ppCode :: (Micro.PrettyCode c) => Micro.Options -> c -> Doc Ann
ppCode opts = runPP opts . Micro.ppCode

ppAtom :: (Micro.PrettyCode c, HasAtomicity c) => Micro.Options -> c -> Doc Ann
ppAtom opts = runPP opts . Micro.ppCodeAtom

runPP :: Micro.Options -> Sem '[Reader Micro.Options] (Doc Micro.Ann) -> Doc Ann
runPP opts = highlight_ . run . runReader opts

highlight_ :: Doc Ann -> Doc Ann
highlight_ = annotate AnnCode

ppApp :: Micro.Options -> (Expression, [(IsImplicit, Expression)]) -> Doc Ann
ppApp opts (fun, args) =
  hsep (ppAtom opts fun : map (uncurry (ppArg opts)) args)

ppArg :: Micro.Options -> IsImplicit -> Expression -> Doc Ann
ppArg opts im arg = case im of
  Implicit -> braces (ppCode opts arg)
  Explicit -> ppAtom opts arg
