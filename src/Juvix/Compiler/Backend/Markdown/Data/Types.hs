module Juvix.Compiler.Backend.Markdown.Data.Types
  ( module Juvix.Compiler.Backend.Markdown.Data.Types,
    module Juvix.Compiler.Backend.Markdown.Data.MkJuvixBlockOptions,
  )
where

import Commonmark qualified as MK
import Data.Text qualified as T
import Juvix.Compiler.Backend.Markdown.Data.MkJuvixBlockOptions
import Juvix.Data.Loc
import Juvix.Prelude hiding (Raw)
import Juvix.Prelude.Pretty
import Text.Show qualified as Show

data JuvixCodeBlock = JuvixCodeBlock
  { _juvixCodeBlock :: Text,
    _juvixCodeBlockOptions :: Text,
    _juvixCodeBlockLoc :: Maybe Interval
  }
  deriving stock (Eq, Ord)

data JuvixExpression = JuvixExpression
  { _juvixExpression :: Text,
    _juvixExpressionOptions :: Text,
    _juvixExpressionLoc :: Maybe Interval
  }
  deriving stock (Eq, Ord, Show)

data Inline
  = InlineTextBlock TextBlock
  | InlineJuvixExpression Text
  deriving stock (Eq, Ord, Show)

newtype Inlines = Inlines
  { _inlines :: [Inline]
  }
  deriving newtype (Semigroup, Monoid, Show, Eq)

data TextBlock = TextBlock
  { _textBlock :: !Text,
    _textBlockLoc :: Maybe Interval
  }
  deriving stock (Eq, Ord)

instance Show TextBlock where
  show TextBlock {..} = T.unpack _textBlock

makeLenses ''JuvixCodeBlock
makeLenses ''TextBlock

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
  | MkInlines Inlines
  | MkNull
  | MkConcat Mk Mk
  deriving stock (Eq, Show)

instance Semigroup TextBlock where
  a <> b =
    TextBlock
      { _textBlock = a ^. textBlock <> b ^. textBlock,
        _textBlockLoc = a ^. textBlockLoc <> b ^. textBlockLoc
      }

instance Monoid TextBlock where
  mempty =
    TextBlock
      { _textBlock = mempty,
        _textBlockLoc = Nothing
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
  -- MkInlines t -> [t ^. textBlock]
  MkInlines {} -> error "TODO"
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
      _textBlockLoc = mempty
    }

toMK :: Text -> Mk
toMK = MkInlines . mkInlinesTextBlock

toMK' :: Text -> Interval -> Mk
toMK' t i =
  MkInlines
    . Inlines
    . pure
    . InlineTextBlock
    $ TextBlock
      { _textBlock = t,
        _textBlockLoc = Just i
      }

wrap' :: Text -> Text -> Inlines -> Inlines
wrap' t1 t2 a = mkInlinesTextBlock t1 <> a <> mkInlinesTextBlock t2

wrap :: Text -> Inlines -> Inlines
wrap t = wrap' t t

paren :: Inlines -> Inlines
paren = wrap' "(" ")"

brack :: Inlines -> Inlines
brack = wrap' "[" "]"

instance MK.HasAttributes Inlines where
  addAttributes _ = id

instance MK.Rangeable Inlines where
  ranged _ r = r

instance MK.HasAttributes Mk where
  addAttributes _ = id

mkInlinesTextBlock :: Text -> Inlines
mkInlinesTextBlock = Inlines . pure . InlineTextBlock . toTextBlock

instance MK.IsInline Inlines where
  lineBreak = mkInlinesTextBlock nl
  softBreak = mkInlinesTextBlock " "
  str = mkInlinesTextBlock
  entity = mkInlinesTextBlock
  escapedChar = mkInlinesTextBlock . T.singleton
  emph = wrap "*"
  strong = wrap "**"
  link dest _ desc =
    brack desc <> paren (mkInlinesTextBlock dest)
  image src _ desc =
    mkInlinesTextBlock "!" <> brack desc <> paren (mkInlinesTextBlock src)
  code = wrap "`" . mkInlinesTextBlock
  rawInline f t
    | f == MK.Format "html" =
        mkInlinesTextBlock t
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
            _juvixCodeBlockLoc = loc
          }
    Nothing ->
      let b = "```" <> info <> t <> "```"
       in MkTextBlock TextBlock {_textBlock = b, _textBlockLoc = loc}

instance MK.IsBlock Inline Mk where
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
              MkInlines tb ->
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
  MkInlines {} -> False
  MkJuvixCodeBlock {} -> False

extractJuvixCodeBlock :: Mk -> [JuvixCodeBlock]
extractJuvixCodeBlock = run . execAccumList . go
  where
    go :: Mk -> Sem '[Accum JuvixCodeBlock] ()
    go = \case
      MkJuvixCodeBlock j -> accum j
      MkConcat a b -> go a <> go b
      MkInlines {} -> return ()
      MkNull -> return ()
