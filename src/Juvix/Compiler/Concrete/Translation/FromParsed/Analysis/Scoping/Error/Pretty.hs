module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error.Pretty
  ( module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error.Pretty,
    module Juvix.Data.CodeAnn,
  )
where

import Juvix.Compiler.Concrete.Pretty.Base qualified as Scoped
import Juvix.Data.CodeAnn
import Juvix.Data.PPOutput
import Juvix.Prelude
import Juvix.Prelude.Pretty
import Text.EditDistance

ppCode :: Scoped.PrettyCode c => c -> Doc Ann
ppCode = runPP . Scoped.ppCode

runPP :: Sem '[Reader Scoped.Options] (Doc Scoped.Ann) -> Doc Ann
runPP = highlight . run . runReader Scoped.defaultOptions

prettyError :: Doc Ann -> AnsiText
prettyError = AnsiText . PPOutput

highlight :: Doc Ann -> Doc Ann
highlight = annotate AnnCode

ppSymbolT :: Text -> Doc Ann
ppSymbolT = highlight . pretty

textDistance :: Text -> Text -> Int
textDistance a b =
  restrictedDamerauLevenshteinDistance
    defaultEditCosts
    (unpack a)
    (unpack b)
