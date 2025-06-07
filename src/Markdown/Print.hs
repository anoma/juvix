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

instance PrettyPrint Strong where
  ppCode (Strong i) = do
    ppCode @Text "**"
    ppCode i
    ppCode @Text "**"

instance PrettyPrint Emph where
  ppCode (Emph i) = do
    ppCode @Text "*"
    ppCode i
    ppCode @Text "*"

-- [link](/uri "title")
instance PrettyPrint Link where
  ppCode Link {..} = do
    ppCode @Text "["
      <> ppCode _linkDescription
      <> ppCode @Text "]("
      <> ppCode _linkDestination
      <+?> ppLinkTitle _linkTitle
        <> ppCode @Text ")"

instance PrettyPrint Image where
  ppCode Image {..} = do
    ppCode @Text "!"
    ppCode
      Link
        { _linkDescription = _imageDescription,
          _linkTitle = _imageTitle,
          _linkDestination = _imageSource
        }

instance PrettyPrint Code where
  ppCode (Code c) = do
    ppCode @Text "`"
    ppCode c
    ppCode @Text "`"

instance PrettyPrint Entity where
  ppCode (Entity e) = ppCode e

instance PrettyPrint Inline where
  ppCode = \case
    InlineString txt -> noLoc (pretty txt)
    InlineSoftBreak b -> ppCode b
    InlineHardBreak b -> ppCode b
    InlineEscapedChar b -> ppCode b
    InlineEntity b -> ppCode b
    InlineEmph b -> ppCode b
    InlineStrong b -> ppCode b
    InlineLink b -> ppCode b
    InlineImage b -> ppCode b
    InlineCode b -> ppCode b
    InlineRaw b -> ppCode b

instance PrettyPrint RawBlock where
  ppCode RawBlock {..} =
    ppCode _rawBlockText

instance PrettyPrint RawInline where
  ppCode RawInline {..} =
    ppCode _rawInlineText

instance PrettyPrint Inlines where
  ppCode = mapM_ ppCode . (^. inlines)

instance PrettyPrint Blocks where
  ppCode =
    concatWith (\a b -> a <> hardline <> hardline <> b)
      . map ppCode
      . (^. blocks)

instance PrettyPrint Text where
  ppCode = noLoc . pretty

instance PrettyPrint CodeBlock where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => CodeBlock -> Sem r ()
  ppCode CodeBlock {..}
    | Text.null _codeBlockLanguage = indentN 4 (ppCode _codeBlock)
    | otherwise = do
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
            indentN 2 (ppCode b)
        )
        _listBlocks

instance PrettyPrint QuoteBlock where
  ppCode QuoteBlock {..} = do
    let q :: Text = toPlainText (ppOut _quoteBlock)
        addQuoteChar :: Text -> Text
        addQuoteChar t = ">" <> t
        withQuotes :: Text =
          Text.dropEnd 1
            . Text.unlines
            . map addQuoteChar
            $ Text.lines q
    noLoc (pretty withQuotes)

ppLinkTitle ::
  (Member (Reader Options) r, Member ExactPrint r) =>
  Text ->
  Maybe (Sem r ())
ppLinkTitle title
  | Text.null title = Nothing
  | otherwise = Just (ppCode (show @Text title))

instance PrettyPrint ReferenceLinkDefinition where
  ppCode ReferenceLinkDefinition {..} = do
    let title = ppLinkTitle _referenceLinkDefinitionTitle
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
    BlockRawBlock p -> ppCode p
    BlockList l -> ppCode l
    BlockReferenceLinkDefinition d -> ppCode d
