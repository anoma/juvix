module Juvix.Compiler.Backend.Markdown.Data.Types where

import Commonmark qualified as MK
import Data.Text qualified as T
import Juvix.Data.Loc
import Juvix.Prelude hiding (Raw)
import Juvix.Prelude.Pretty
import Text.Show qualified as Show

newtype MkJuvixBlockOptions = MkJuvixBlockOptions
  { _mkJuvixBlockOptionsHide :: Bool
  }
  deriving stock (Eq, Ord)

data JuvixCodeBlock = JuvixCodeBlock
  { _juvixCodeBlock :: Text,
    _juvixCodeBlockOptions :: MkJuvixBlockOptions,
    _juvixCodeBlockInterval :: Maybe Interval
  }
  deriving stock (Eq, Ord)

data TextBlock = TextBlock
  { _textBlock :: !Text,
    _textBlockInterval :: Maybe Interval
  }
  deriving stock (Eq, Ord)

makeLenses ''JuvixCodeBlock
makeLenses ''MkJuvixBlockOptions
makeLenses ''TextBlock

defaultMkJuvixBlockOptions :: MkJuvixBlockOptions
defaultMkJuvixBlockOptions =
  MkJuvixBlockOptions
    { _mkJuvixBlockOptionsHide = False
    }

instance Show TextBlock where
  show t = T.unpack (t ^. textBlock)

textJuvixBlockOptions :: MkJuvixBlockOptions -> Text
textJuvixBlockOptions opt =
  T.intercalate " " $
    catMaybes
      [ if opt ^. mkJuvixBlockOptionsHide then Just "hide" else Nothing
      ]

instance Show MkJuvixBlockOptions where
  show opt = T.unpack (textJuvixBlockOptions opt)

textJuvixCodeBlock :: JuvixCodeBlock -> Text
textJuvixCodeBlock cb =
  mconcat
    [ "```juvix",
      textJuvixBlockOptions (cb ^. juvixCodeBlockOptions),
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
  deriving stock (Eq, Show, Ord)

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

instance Semigroup MkJuvixBlockOptions where
  a <> b =
    MkJuvixBlockOptions
      { _mkJuvixBlockOptionsHide = a ^. mkJuvixBlockOptionsHide || b ^. mkJuvixBlockOptionsHide
      }

instance Monoid MkJuvixBlockOptions where
  mempty =
    MkJuvixBlockOptions
      { _mkJuvixBlockOptionsHide = False
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

instance MK.ToPlainText TextBlock where
  toPlainText r = r ^. textBlock

instance MK.ToPlainText JuvixCodeBlock where
  toPlainText = show

instance MK.ToPlainText Mk where
  toPlainText =
    trimText
      . mconcat
      . builder

builder :: Mk -> [Text]
builder = \case
  MkConcat a b -> builder a <> builder b
  MkTextBlock t -> [trimText (t ^. textBlock) <> nl]
  MkJuvixCodeBlock j -> [textJuvixCodeBlock j]
  MkNull -> mempty

flatten :: [Mk] -> Mk
flatten = foldl' (<>) MkNull

instance MK.Rangeable Mk where
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

instance MK.HasAttributes TextBlock where
  addAttributes _ = id

instance MK.Rangeable TextBlock where
  ranged _ r = r

instance MK.HasAttributes Mk where
  addAttributes _ = id

instance MK.IsInline TextBlock where
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
    | f == MK.Format "html" =
        toTextBlock t
    | otherwise = mempty

getJuvixBlockOptions :: Text -> MkJuvixBlockOptions
getJuvixBlockOptions = \case
  "hide" -> mempty {_mkJuvixBlockOptionsHide = True}
  _ -> mempty

nl' :: Mk
nl' = toMK nl

processCodeBlock :: Text -> Text -> Maybe Interval -> Mk
processCodeBlock info t loc =
  case T.splitOn " " (T.strip info) of
    ("juvix" : opts) ->
      MkJuvixCodeBlock
        JuvixCodeBlock
          { _juvixCodeBlock = t,
            _juvixCodeBlockOptions = foldMap getJuvixBlockOptions opts,
            _juvixCodeBlockInterval = loc
          }
    _ ->
      let b = "```" <> info <> nl <> t <> "```"
       in MkTextBlock TextBlock {_textBlock = b, _textBlockInterval = loc}

instance-- (MK.IsInline TextBlock) =>
  MK.IsBlock TextBlock Mk where
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
