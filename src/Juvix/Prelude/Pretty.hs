module Juvix.Prelude.Pretty
  ( module Juvix.Prelude.Pretty,
    module Prettyprinter,
    module Prettyprinter.Render.Terminal,
    module Prettyprinter.Util,
    module Prettyprinter.Render.Text,
  )
where

import Data.Text qualified as Text
import Juvix.Prelude.Base
import Prettyprinter hiding (concatWith, defaultLayoutOptions, hsep, sep, vsep)
import Prettyprinter qualified as PP
import Prettyprinter.Render.Terminal (AnsiStyle)
import Prettyprinter.Render.Terminal qualified as Ansi
import Prettyprinter.Render.Text (renderStrict)
import Prettyprinter.Render.Text qualified as Text
import Prettyprinter.Util (reflow)
import Prelude

-- | The page width is 150 with the desired length (not counting indent spaces)
-- being 150*0.4 = 60
defaultLayoutOptions :: LayoutOptions
defaultLayoutOptions =
  LayoutOptions
    { layoutPageWidth = AvailablePerLine 150 0.4
    }

class HasAnsiBackend a where
  toAnsiStream :: a -> SimpleDocStream Ansi.AnsiStyle
  toAnsiStream = layoutPretty defaultLayoutOptions . toAnsiDoc

  toAnsiDoc :: a -> Doc Ansi.AnsiStyle

class HasTextBackend a where
  toTextStream :: a -> SimpleDocStream b
  toTextStream = layoutPretty defaultLayoutOptions . toTextDoc

  toTextDoc :: a -> Doc b

data AnsiTextAtom = forall t.
  (HasAnsiBackend t, HasTextBackend t) =>
  AnsiTextAtom
  { _ansiTextAtom :: t
  }

newtype AnsiText = AnsiText
  { _ansiTextAtoms :: [AnsiTextAtom]
  }
  deriving newtype (Semigroup, Monoid)

mkAnsiText ::
  (HasAnsiBackend t, HasTextBackend t) =>
  t ->
  AnsiText
mkAnsiText = AnsiText . pure . AnsiTextAtom

makeLenses ''AnsiText

instance HasTextBackend String where
  toTextStream = toTextStream . pretty
  toTextDoc = toTextDoc . pretty

instance HasTextBackend Text where
  toTextStream = toTextStream . pretty
  toTextDoc = toTextDoc . pretty

instance HasAnsiBackend String where
  toAnsiDoc = pretty

instance HasAnsiBackend Text where
  toAnsiDoc = pretty

instance HasTextBackend AnsiTextAtom where
  toTextStream (AnsiTextAtom t) = toTextStream t
  toTextDoc (AnsiTextAtom t) = toTextDoc t

instance HasAnsiBackend AnsiTextAtom where
  toAnsiStream (AnsiTextAtom t) = toAnsiStream t
  toAnsiDoc (AnsiTextAtom t) = toAnsiDoc t

instance HasTextBackend AnsiText where
  toTextDoc :: AnsiText -> Doc b
  toTextDoc (AnsiText l) = mconcatMap toTextDoc l

instance HasAnsiBackend AnsiText where
  toAnsiDoc (AnsiText l) = mconcatMap toAnsiDoc l

instance HasTextBackend (Doc a) where
  toTextDoc = unAnnotate
  toTextStream = layoutPretty defaultLayoutOptions . unAnnotate

instance HasAnsiBackend (Doc Ansi.AnsiStyle) where
  toAnsiDoc = id
  toAnsiStream = layoutPretty defaultLayoutOptions

instance Show AnsiTextAtom where
  show (AnsiTextAtom t) = unpack (Text.renderStrict (toTextStream t))

instance Pretty AnsiTextAtom where
  pretty (AnsiTextAtom t) = pretty (Text.renderStrict (toTextStream t))

ansiTextToText :: AnsiText -> Text
ansiTextToText = Text.renderStrict . layoutPretty defaultLayoutOptions . mconcatMap toAnsiDoc . (^. ansiTextAtoms)

instance Show AnsiText where
  show = unpack . ansiTextToText

instance Pretty AnsiText where
  pretty = pretty . ansiTextToText

renderIO :: (MonadIO m, HasAnsiBackend a, HasTextBackend a) => Bool -> a -> m ()
renderIO useColors = hRenderIO useColors stdout

hRenderIO :: (MonadIO m, HasAnsiBackend a, HasTextBackend a) => Bool -> Handle -> a -> m ()
hRenderIO useColors h
  | useColors = liftIO . Ansi.renderIO h . toAnsiStream
  | otherwise = liftIO . Text.renderIO h . toTextStream

toAnsiText :: (HasAnsiBackend a, HasTextBackend a) => Bool -> a -> Text
toAnsiText useColors
  | useColors = Ansi.renderStrict . toAnsiStream
  | otherwise = Text.renderStrict . toTextStream

toPlainText :: (HasTextBackend a) => a -> Text
toPlainText = Text.renderStrict . toTextStream

trimText :: Text -> Text
trimText =
  Text.unlines
    . map Text.stripEnd
    . dropWhileEnd Text.null
    . dropWhile Text.null
    . Text.lines

toPlainTextTrim :: (HasTextBackend a) => a -> Text
toPlainTextTrim = trimText . toPlainText

prettyText :: (Pretty a) => a -> Text
prettyText = Text.renderStrict . layoutPretty defaultLayoutOptions . pretty

hsepSoft' :: (Foldable f) => f (Doc a) -> Doc a
hsepSoft' = concatWith (\a b -> a <> softline' <> b)

vsepHard :: (Foldable f) => f (Doc a) -> Doc a
vsepHard = concatWith (\a b -> a <> hardline <> b)

vsep :: (Foldable f) => f (Doc a) -> Doc a
vsep = PP.vsep . toList

vsep2 :: (Foldable f) => f (Doc a) -> Doc a
vsep2 = concatWith (\a b -> a <> line <> line <> b)

hsep :: (Foldable f) => f (Doc a) -> Doc a
hsep = PP.hsep . toList

sep :: (Foldable f) => f (Doc a) -> Doc a
sep = PP.sep . toList

enclose1 :: Doc a -> Doc a -> Doc a
enclose1 delim = enclose delim delim

vsepMaybe :: (Foldable f) => f (Doc a) -> Maybe (Doc a)
vsepMaybe l
  | null l = Nothing
  | otherwise = Just (vsep l)

hsepMaybe :: (Foldable f) => f (Doc a) -> Maybe (Doc a)
hsepMaybe l
  | null l = Nothing
  | otherwise = Just (hsep l)

nest' :: Doc a -> Doc a
nest' = nest 2

indent' :: Doc a -> Doc a
indent' = indent 2

hang' :: Doc a -> Doc a
hang' = hang 2

spaceOrEmpty :: Doc a
spaceOrEmpty = flatAlt (pretty ' ') mempty

oneLineOrNext :: Doc a -> Doc a
oneLineOrNext x = PP.group (flatAlt (line <> indent' x) (space <> x))

oneLineOrNextBrackets :: Doc a -> Doc a
oneLineOrNextBrackets = oneLineOrNextDelims lbracket rbracket

oneLineOrNextNoIndent :: Doc a -> Doc a
oneLineOrNextNoIndent x = PP.group (flatAlt (line <> x) (space <> x))

oneLineOrNextBlock :: Doc a -> Doc a
oneLineOrNextBlock x = PP.group (flatAlt (line <> indent' x <> line) (space <> x <> space))

oneLineOrNextDelims :: Doc a -> Doc a -> Doc a -> Doc a
oneLineOrNextDelims l r x = PP.group (flatAlt (l <> line <> indent' x <> line <> r) (l <> x <> r))

oneLineOrNextBraces :: Doc a -> Doc a
oneLineOrNextBraces = oneLineOrNextDelims lbrace rbrace

nextLine :: Doc a -> Doc a
nextLine x = PP.group (line <> x)

ordinal :: Int -> Doc a
ordinal = \case
  1 -> "first"
  2 -> "second"
  3 -> "third"
  4 -> "fourth"
  5 -> "fifth"
  6 -> "sixth"
  7 -> "seventh"
  8 -> "eighth"
  9 -> "ninth"
  10 -> "tenth"
  11 -> "eleventh"
  12 -> "twelfth"
  n -> pretty n <> "th"

isVowel :: Char -> Bool
isVowel = (`elem` ("aeiouAEIOU" :: [Char]))

withArticle :: Text -> Text
withArticle n = articleFor n <> " " <> n

articleFor :: Text -> Text
articleFor n
  | Text.null n = ""
  | isVowel (Text.head n) = "an"
  | otherwise = "a"

itemize :: (Functor f, Foldable f) => f (Doc a) -> Doc a
itemize = vsep . fmap ("• " <>)
