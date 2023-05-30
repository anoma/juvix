module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error.Pretty
  ( module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error.Pretty,
    module Juvix.Data.CodeAnn,
  )
where

import Juvix.Compiler.Concrete.Pretty.Base qualified as Scoped
import Juvix.Data.CodeAnn
import Juvix.Data.PPOutput
import Juvix.Prelude
import Text.EditDistance

ppCode :: (Scoped.PrettyCode c) => Scoped.Options -> c -> Doc Ann
ppCode opts = runPP opts . Scoped.ppCode

ppMessage :: (Scoped.PrettyCode c) => Scoped.Options -> c -> Doc Ann
ppMessage opts = runPP' opts . Scoped.ppCode

runPP' :: Scoped.Options -> Sem '[Reader Scoped.Options] (Doc Scoped.Ann) -> Doc Ann
runPP' opts = run . runReader opts

runPP :: Scoped.Options -> Sem '[Reader Scoped.Options] (Doc Scoped.Ann) -> Doc Ann
runPP opts = code . runPP' opts

prettyError :: Doc Ann -> AnsiText
prettyError = mkAnsiText . PPOutput

ppSymbolT :: Text -> Doc Ann
ppSymbolT = code . pretty

textDistance :: Text -> Text -> Int
textDistance a b =
  restrictedDamerauLevenshteinDistance
    defaultEditCosts
    (unpack a)
    (unpack b)
