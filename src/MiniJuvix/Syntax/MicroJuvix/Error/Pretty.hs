module MiniJuvix.Syntax.MicroJuvix.Error.Pretty
  ( module MiniJuvix.Syntax.MicroJuvix.Error.Pretty,
    module MiniJuvix.Syntax.MicroJuvix.Error.Pretty.Ann,
    module MiniJuvix.Prelude.Pretty,
  )
where

import MiniJuvix.Prelude
import MiniJuvix.Prelude.Pretty
import MiniJuvix.Syntax.Fixity
import MiniJuvix.Syntax.MicroJuvix.Error.Pretty.Ann
import MiniJuvix.Syntax.MicroJuvix.Error.Pretty.Ansi qualified as Ansi
import MiniJuvix.Syntax.MicroJuvix.Language.Extra
import MiniJuvix.Syntax.MicroJuvix.Pretty.Base qualified as Micro

ppCode :: Micro.PrettyCode c => c -> Doc Eann
ppCode = runPP . Micro.ppCode

ppAtom :: (Micro.PrettyCode c, HasAtomicity c) => c -> Doc Eann
ppAtom = runPP . Micro.ppCodeAtom

runPP :: Sem '[Reader Micro.Options] (Doc Micro.Ann) -> Doc Eann
runPP = highlight_ . reAnnotate MicroAnn . run . runReader Micro.defaultOptions

newtype PPOutput = PPOutput (Doc Eann)

prettyError :: Doc Eann -> AnsiText
prettyError = AnsiText . PPOutput

instance HasAnsiBackend PPOutput where
  toAnsiStream (PPOutput o) = reAnnotateS Ansi.stylize (layoutPretty defaultLayoutOptions o)
  toAnsiDoc (PPOutput o) = reAnnotate Ansi.stylize o

instance HasTextBackend PPOutput where
  toTextDoc (PPOutput o) = unAnnotate o
  toTextStream (PPOutput o) = unAnnotateS (layoutPretty defaultLayoutOptions o)

indent' :: Doc ann -> Doc ann
indent' = indent 2

highlight_ :: Doc Eann -> Doc Eann
highlight_ = annotate Highlight

ppApp :: (Expression, [(IsImplicit, Expression)]) -> Doc Eann
ppApp (fun, args) =
  hsep (ppAtom fun : map (uncurry ppArg) args)

ppArg :: IsImplicit -> Expression -> Doc Eann
ppArg im arg = case im of
  Implicit -> braces (ppCode arg)
  Explicit -> ppAtom arg
