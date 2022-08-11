module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error.Pretty
  ( module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error.Pretty,
    module Juvix.Data.CodeAnn,
  )
where

import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Pretty.Base qualified as Micro
import Juvix.Data.CodeAnn
import Juvix.Prelude

ppCode :: Micro.PrettyCode c => c -> Doc Ann
ppCode = runPP . Micro.ppCode

ppAtom :: (Micro.PrettyCode c, HasAtomicity c) => c -> Doc Ann
ppAtom = runPP . Micro.ppCodeAtom

runPP :: Sem '[Reader Micro.Options] (Doc Micro.Ann) -> Doc Ann
runPP = highlight_ . run . runReader Micro.defaultOptions

highlight_ :: Doc Ann -> Doc Ann
highlight_ = annotate AnnCode

ppApp :: (Expression, [(IsImplicit, Expression)]) -> Doc Ann
ppApp (fun, args) =
  hsep (ppAtom fun : map (uncurry ppArg) args)

ppArg :: IsImplicit -> Expression -> Doc Ann
ppArg im arg = case im of
  Implicit -> braces (ppCode arg)
  Explicit -> ppAtom arg
