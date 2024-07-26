module Juvix.Compiler.Backend.Latex.Translation.FromScoped.Source where

import Data.Text qualified as Text
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder qualified as Builder
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Print
import Juvix.Prelude
import Juvix.Prelude.Pretty
import Prettyprinter.Render.Util.SimpleDocTree

data LatexColor
  = JuInductive
  | JuConstructor
  | JuFunction
  | JuAxiom
  | JuString
  | JuKeyword
  | JuDelimiter
  | JuVar
  | JuFixity
  | JuNumber
  | JuComment
  | JuJudoc

genSourceLatex :: forall m. (MonadIO m) => Module 'Scoped 'ModuleTop -> m ()
genSourceLatex = putStrLn . genModuleText

genModuleText ::
  Module 'Scoped 'ModuleTop ->
  Text
genModuleText = toStrict . Builder.toLazyText . genModuleLatex

genModuleLatex ::
  Module 'Scoped 'ModuleTop ->
  TextBuilder
genModuleLatex =
  renderTree
    . treeForm
    . layoutPretty defaultLayoutOptions
    . docNoComments defaultOptions

renderTree :: SimpleDocTree Ann -> TextBuilder
renderTree = go

go :: SimpleDocTree Ann -> TextBuilder
go sdt = case sdt of
  STEmpty -> mempty
  STChar c -> Builder.singleton c
  STText _ (t :: Text) -> Builder.fromText t
  STLine n -> ("\n" <> textSpaces n)
  STAnn ann content -> putTag ann (go content)
  STConcat l -> mconcatMap go l
  where
    textSpaces :: Int -> TextBuilder
    textSpaces n = fromText (Text.replicate n (Text.singleton ' '))

juColor :: LatexColor -> TextBuilder -> TextBuilder
juColor c txt = "\\textcolor{" <> toStr c <> "}{" <> txt <> "}"
  where
    toStr :: LatexColor -> TextBuilder
    toStr = \case
      JuInductive -> "ju-inductive"
      JuConstructor -> "ju-constructor"
      JuFunction -> "ju-function"
      JuComment -> "ju-comment"
      JuJudoc -> "ju-judoc"
      JuAxiom -> "ju-axiom"
      JuString -> "ju-string"
      JuKeyword -> "ju-keyword"
      JuDelimiter -> "ju-delimiter"
      JuFixity -> "ju-fixity"
      JuVar -> "ju-var"
      JuNumber -> "ju-number"

juKindColor :: S.NameKind -> LatexColor
juKindColor = \case
  S.KNameConstructor -> JuConstructor
  S.KNameInductive -> JuInductive
  S.KNameFunction -> JuFunction
  S.KNameLocal -> JuVar
  S.KNameAxiom -> JuAxiom
  S.KNameLocalModule -> JuVar
  S.KNameAlias -> JuVar
  S.KNameTopModule -> JuVar
  S.KNameFixity -> JuFixity

putTag :: Ann -> TextBuilder -> TextBuilder
putTag ann x = case ann of
  AnnKind k -> juColor (juKindColor k) x
  AnnLiteralInteger -> juColor JuNumber x
  AnnLiteralString -> juColor JuString x
  AnnKeyword -> juColor JuKeyword x
  AnnUnkindedSym -> juColor JuVar x
  AnnComment -> juColor JuComment x
  AnnJudoc -> juColor JuJudoc x
  AnnDelimiter -> juColor JuDelimiter x
  AnnDef {} -> x
  AnnRef {} -> x
  AnnCode -> x
  AnnImportant -> x
