module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error.Pretty
  ( module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error.Pretty,
    module Juvix.Data.CodeAnn,
  )
where

import Juvix.Compiler.Concrete.Print.Base qualified as Scoped
import Juvix.Data.CodeAnn
import Juvix.Data.PPOutput
import Juvix.Prelude
import Text.EditDistance

ppCode :: Scoped.PrettyPrint c => Scoped.Options -> c -> Doc Ann
ppCode opts = code . ppMessage opts

ppMessage :: Scoped.PrettyPrint c => Scoped.Options -> c -> Doc Ann
ppMessage = Scoped.docNoComments

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
