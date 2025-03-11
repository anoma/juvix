module Markdown.Print
  ( module Markdown.Print,
    module Markdown.Print.Options,
  )
where

import Data.Text qualified as Text
import Juvix.Data.Effect.ExactPrint
import Juvix.Data.PPOutput (AnsiText, mkAnsiText)
import Juvix.Prelude
import Juvix.Prelude.Pretty (pretty)
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

instance PrettyPrint Inline where
  ppCode = \case
    InlineString txt -> noLoc (pretty txt)
    InlineSoftBreak -> hardline
    _ -> todo

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

instance PrettyPrint Block where
  ppCode = \case
    BlockParagraph p -> ppCode p
    BlockPlain p -> ppCode p
    BlockCodeBlock p -> ppCode p
    BlockHeading p -> ppCode p
    x -> error ("TODO: " <> show x)
