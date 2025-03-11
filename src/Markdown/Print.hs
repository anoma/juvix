module Markdown.Print
  ( module Markdown.Print,
    module Markdown.Print.Options,
  )
where

import Data.Stream qualified as Stream
import Data.Text qualified as Text
import Juvix.Data.Effect.ExactPrint
import Juvix.Prelude.Base hiding ((<+>), (<+?>))
import Juvix.Prelude.Pretty (AnsiText, mkAnsiText, pretty, prettyText, toPlainText)
import Markdown.Language
import Markdown.Print.Options

ppOut :: (PrettyPrint c) => c -> AnsiText
ppOut =
  mkAnsiText
    . run
    . execExactPrint Nothing
    . runReader Options
    . ppCode

class PrettyPrint a where
  ppCode :: (Members '[ExactPrint, Reader Options] r) => a -> Sem r ()

instance (PrettyPrint a) => PrettyPrint (Meta a) where
  ppCode = ppCode . (^. metaArg)

instance PrettyPrint SoftBreak where
  ppCode _ = hardline

instance PrettyPrint HardBreak where
  ppCode _ = do
    ppCode @Text "\\"
    hardline

instance PrettyPrint EscapedChar where
  ppCode (EscapedChar c) =
    ppCode (showEscapedChar c)

instance PrettyPrint Inline where
  ppCode = \case
    InlineString txt -> noLoc (pretty txt)
    InlineSoftBreak b -> ppCode b
    InlineHardBreak b -> ppCode b
    InlineEscapedChar b -> ppCode b

instance PrettyPrint Inlines where
  ppCode = mapM_ ppCode . (^. inlines)

instance PrettyPrint Blocks where
  ppCode = concatWith (\a b -> a <> hardline <> hardline <> b) . map ppCode . (^. blocks)

instance PrettyPrint Text where
  ppCode = noLoc . pretty

instance PrettyPrint CodeBlock where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => CodeBlock -> Sem r ()
  ppCode CodeBlock {..} = do
    codeSep
    ppCode _codeBlockLanguage
    hardline
    ppCode _codeBlock
    codeSep
    where
      codeSep :: Sem r ()
      codeSep = ppCode @Text "```"

instance PrettyPrint Heading where
  ppCode Heading {..} = do
    ppCode (Text.replicate _headingLevel "#")
    if
        | null (_headingText ^. inlines) -> return ()
        | otherwise -> ppCode @Text " " >> ppCode _headingText

instance PrettyPrint ThematicBreak where
  ppCode ThematicBreak = ppCode @Text "---"

instance PrettyPrint List where
  ppCode List {..} = do
    let stream :: Stream Text = case _listType of
          BulletList (bullet :: Char) -> Stream.repeat (Text.singleton bullet <> " ")
          OrderedList (start :: Int) (_enumType :: EnumeratorType) (delimType :: DelimiterType) ->
            let addDelim :: Text -> Text
                addDelim num =
                  case delimType of
                    Period -> num <> ". "
                    OneParen -> num <> ") "
                    TwoParens -> "(" <> num <> ") "
             in addDelim . prettyText <$> allNaturalsFrom (fromIntegral start)

    runStreamOf stream . vsepHard $
      fmap
        ( \b -> do
            num <- yield @Text
            ppCode num
            ppCode b
        )
        _listBlocks

instance PrettyPrint QuoteBlock where
  ppCode QuoteBlock {..} = do
    let q :: Text = toPlainText (ppOut _quoteBlock)
        withQuotes :: Text =
          Text.dropEnd 1
            . Text.unlines
            . map ("> " <>)
            $ Text.lines q
    noLoc (pretty withQuotes)

instance PrettyPrint ReferenceLinkDefinition where
  ppCode ReferenceLinkDefinition {..} = do
    let title
          | Text.null _referenceLinkDefinitionTitle = Nothing
          | otherwise = Just (ppCode (show @Text _referenceLinkDefinitionTitle))
    ppCode ("[" <> _referenceLinkDefinitionLabel <> "]:")
      <+> ppCode _referenceLinkDefinitionDestination
      <+?> title

instance PrettyPrint Block where
  ppCode = \case
    BlockParagraph p -> ppCode p
    BlockPlain p -> ppCode p
    BlockCodeBlock p -> ppCode p
    BlockHeading p -> ppCode p
    BlockThematicBreak p -> ppCode p
    BlockQuote p -> ppCode p
    BlockRawBlock {} -> error "raw block"
    BlockList l -> ppCode l
    BlockReferenceLinkDefinition d -> ppCode d
