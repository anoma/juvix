module Juvix.Compiler.Backend.Latex.Translation.FromScoped.Source
  ( concreteToLatex,
  )
where

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
  | JuModule

concreteToLatex ::
  (PrettyPrint a) =>
  Maybe FileComments ->
  a ->
  Text
concreteToLatex c = toStrict . Builder.toLazyText . genLatex c

genLatex :: (PrettyPrint a) => Maybe FileComments -> a -> TextBuilder
genLatex c =
  renderTree
    . treeForm
    . layoutPretty defaultLayoutOptions
    . docHelper c defaultOptions

renderTree :: SimpleDocTree Ann -> TextBuilder
renderTree = go

go :: SimpleDocTree Ann -> TextBuilder
go sdt = case sdt of
  STEmpty -> mempty
  STChar c -> Builder.fromText (escapeCharacter c)
  STText _ (t :: Text) -> Builder.fromText (escape t)
  STLine n -> "\n" <> textSpaces n
  STAnn ann content -> putTag ann (go content)
  STConcat l -> mconcatMap go l
  where
    escapeCharacter :: Char -> Text
    escapeCharacter = \case
      '\\' -> "\\\\"
      '{' -> "\\{"
      '}' -> "\\}"
      c -> Text.singleton c

    escape :: Text -> Text
    escape = Text.concatMap escapeCharacter

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
      JuModule -> "ju-module"

juKindColor :: S.NameKind -> LatexColor
juKindColor = \case
  S.KNameConstructor -> JuConstructor
  S.KNameInductive -> JuInductive
  S.KNameFunction -> JuFunction
  S.KNameLocal -> JuVar
  S.KNameAxiom -> JuAxiom
  S.KNameLocalModule -> JuModule
  S.KNameAlias -> JuVar
  S.KNameTopModule -> JuModule
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
  AnnPragma -> juColor JuComment x
  AnnDef {} -> x
  AnnRef {} -> x
  AnnCode -> x
  AnnImportant -> x
