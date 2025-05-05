module Juvix.Compiler.Backend.Markdown.Data.Types
  ( module Juvix.Compiler.Backend.Markdown.Data.Types,
    module Juvix.Compiler.Backend.Markdown.Data.MkJuvixBlockOptions,
  )
where

import Commonmark qualified as CM
import Data.Text qualified as T
import Juvix.Compiler.Backend.Markdown.Data.MkJuvixBlockOptions
import Juvix.Data.Loc
import Juvix.Prelude hiding (Raw)
import Juvix.Prelude.Pretty
import Text.Show qualified as Show

data JuvixCodeBlock = JuvixCodeBlock
  { _juvixCodeBlock :: Text,
    _juvixCodeBlockOptions :: Text,
    _juvixCodeBlockInterval :: Maybe Interval
  }
  deriving stock (Eq, Ord, Generic)

instance Serialize JuvixCodeBlock

instance NFData JuvixCodeBlock

data TextBlock = TextBlock
  { _textBlock :: !Text,
    _textBlockInterval :: Maybe Interval
  }
  deriving stock (Eq, Ord, Generic)

instance Serialize TextBlock

instance NFData TextBlock

makeLenses ''JuvixCodeBlock
makeLenses ''TextBlock

instance Show TextBlock where
  show t = T.unpack (t ^. textBlock)

textJuvixCodeBlock :: JuvixCodeBlock -> Text
textJuvixCodeBlock cb =
  mconcat
    [ "```juvix",
      cb ^. juvixCodeBlockOptions,
      nl,
      cb ^. juvixCodeBlock,
      "```"
    ]

instance Show JuvixCodeBlock where
  show cb = T.unpack (textJuvixCodeBlock cb)

data Mk
  = MkJuvixCodeBlock JuvixCodeBlock
  | MkTextBlock TextBlock
  | MkNull
  | MkConcat Mk Mk
  deriving stock (Eq, Show, Ord, Generic)

instance Serialize Mk

instance NFData Mk

instance Semigroup TextBlock where
  a <> b =
    TextBlock
      { _textBlock = a ^. textBlock <> b ^. textBlock,
        _textBlockInterval = a ^. textBlockInterval <> b ^. textBlockInterval
      }

instance Monoid TextBlock where
  mempty =
    TextBlock
      { _textBlock = mempty,
        _textBlockInterval = Nothing
      }
  mappend = (<>)

instance Semigroup Mk where
  a <> MkNull = a
  MkNull <> a = a
  a <> b = MkConcat a b

instance Monoid Mk where
  mempty = MkNull
  mappend = (<>)

nl :: Text
nl = "\n"

instance CM.ToPlainText TextBlock where
  toPlainText r = r ^. textBlock

instance CM.ToPlainText JuvixCodeBlock where
  toPlainText = show

instance CM.ToPlainText Mk where
  toPlainText =
    trimText
      . mconcat
      . builder

builder :: Mk -> [Text]
builder = \case
  MkConcat a b -> builder a <> builder b
  MkTextBlock t -> [t ^. textBlock]
  MkJuvixCodeBlock j -> [textJuvixCodeBlock j]
  MkNull -> mempty

flatten :: [Mk] -> Mk
flatten = foldl' (<>) MkNull

instance CM.Rangeable Mk where
  ranged _ x = x

toTextBlock :: Text -> TextBlock
toTextBlock t =
  TextBlock
    { _textBlock = t,
      _textBlockInterval = mempty
    }

toMK :: Text -> Mk
toMK = MkTextBlock . toTextBlock

toMK' :: Text -> Interval -> Mk
toMK' t i =
  MkTextBlock
    TextBlock
      { _textBlock = t,
        _textBlockInterval = Just i
      }

wrap' :: Text -> Text -> TextBlock -> TextBlock
wrap' t1 t2 a = toTextBlock t1 <> a <> toTextBlock t2

wrap :: Text -> TextBlock -> TextBlock
wrap t = wrap' t t

paren :: TextBlock -> TextBlock
paren = wrap' "(" ")"

brack :: TextBlock -> TextBlock
brack = wrap' "[" "]"

instance CM.HasAttributes TextBlock where
  addAttributes _ = id

instance CM.Rangeable TextBlock where
  ranged _ r = r

instance CM.HasAttributes Mk where
  addAttributes _ = id

instance CM.IsInline TextBlock where
  lineBreak = toTextBlock nl
  softBreak = toTextBlock " "
  str = toTextBlock
  entity = toTextBlock
  escapedChar = toTextBlock . T.singleton
  emph = wrap "*"
  strong = wrap "**"
  link dest _ desc =
    brack desc <> paren (toTextBlock dest)
  image src _ desc =
    toTextBlock "!" <> brack desc <> paren (toTextBlock src)
  code = wrap "`" . toTextBlock
  rawInline f t
    | f == CM.Format "html" =
        toTextBlock t
    | otherwise = mempty

nl' :: Mk
nl' = toMK nl

processCodeBlock :: Text -> Text -> Maybe Interval -> Mk
processCodeBlock info t loc =
  case T.stripPrefix "juvix" (T.strip info) of
    Just opts ->
      MkJuvixCodeBlock
        JuvixCodeBlock
          { _juvixCodeBlock = t,
            _juvixCodeBlockOptions = opts,
            _juvixCodeBlockInterval = loc
          }
    Nothing ->
      let b = "```" <> info <> t <> "```"
       in MkTextBlock TextBlock {_textBlock = b, _textBlockInterval = loc}

instance CM.IsBlock TextBlock Mk where
  paragraph a = MkTextBlock a
  plain a = MkTextBlock a
  thematicBreak = toMK "---"
  blockQuote p = toMK "> " <> p
  heading n t = toMK $ (T.replicate n "#") <> " " <> t ^. textBlock
  rawBlock _ t = toMK t
  codeBlock i t = processCodeBlock i t mempty
  referenceLinkDefinition _ _ = mempty
  list _ _ xs =
    mconcat
      ( map
          ( \b -> case b of
              MkTextBlock tb ->
                MkTextBlock
                  (tb {_textBlock = "- " <> tb ^. textBlock})
              _ -> b
          )
          xs
      )

nullMk :: Mk -> Bool
nullMk = \case
  MkConcat a b -> nullMk a && nullMk b
  MkNull -> True
  _ -> False

extractJuvixCodeBlock :: Mk -> [JuvixCodeBlock]
extractJuvixCodeBlock = \case
  MkJuvixCodeBlock j -> [j]
  MkConcat a b -> extractJuvixCodeBlock a <> extractJuvixCodeBlock b
  _ -> []
