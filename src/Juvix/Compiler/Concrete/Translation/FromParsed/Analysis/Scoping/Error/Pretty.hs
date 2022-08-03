-- TODO: MOVE
module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error.Pretty
  ( module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error.Pretty,
    module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error.Ann,
  )
where

import Juvix.Compiler.Concrete.Pretty.Base qualified as Scoped
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error.Ann
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error.Pretty.Ansi qualified as Ansi
import Juvix.Prelude
import Juvix.Prelude.Pretty
import Text.EditDistance

ppCode :: Scoped.PrettyCode c => c -> Doc Eann
ppCode = runPP . Scoped.ppCode

runPP :: Sem '[Reader Scoped.Options] (Doc Scoped.Ann) -> Doc Eann
runPP = highlight . reAnnotate ScopedAnn . run . runReader Scoped.defaultOptions

newtype PPOutput = PPOutput (Doc Eann)

prettyError :: Doc Eann -> AnsiText
prettyError = AnsiText . PPOutput

instance HasAnsiBackend PPOutput where
  toAnsiStream (PPOutput o) = reAnnotateS Ansi.stylize (layoutPretty defaultLayoutOptions o)
  toAnsiDoc (PPOutput o) = reAnnotate Ansi.stylize o

instance HasTextBackend PPOutput where
  toTextDoc (PPOutput o) = unAnnotate o
  toTextStream (PPOutput o) = unAnnotateS (layoutPretty defaultLayoutOptions o)

highlight :: Doc Eann -> Doc Eann
highlight = annotate Highlight

ppSymbolT :: Text -> Doc Eann
ppSymbolT = highlight . pretty

indent' :: Doc ann -> Doc ann
indent' = indent 2

textDistance :: Text -> Text -> Int
textDistance a b =
  restrictedDamerauLevenshteinDistance
    defaultEditCosts
    (unpack a)
    (unpack b)
