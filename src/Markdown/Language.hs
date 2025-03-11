module Markdown.Language
  ( module Markdown.Language,
    Attribute,
    Format (..),
    ListSpacing (..),
    EnumeratorType (..),
    DelimiterType (..),
    ListType (..),
  )
where

import Commonmark
import Juvix.Prelude
import Juvix.Prelude.Pretty

data Link = Link
  { _linkDestination :: Text,
    _linkTitle :: Text,
    _linkDescription :: Inlines
  }
  deriving stock (Show, Eq, Generic)

data Image = Image
  { _imageSource :: Text,
    _imageTitle :: Text,
    _imageDescription :: Inlines
  }
  deriving stock (Show, Eq, Generic)

data RawInline = RawInline
  { _rawInlineFormat :: Format,
    _rawInlineText :: Text
  }
  deriving stock (Show, Eq, Generic)

data Meta a = Meta
  { _metaAttributes :: [Attribute],
    _metaLoc :: Irrelevant SourceRange,
    _metaArg :: a
  }
  deriving stock (Show, Eq, Generic)

data HardBreak = HardBreak
  deriving stock (Show, Eq, Generic)

data SoftBreak = SoftBreak
  deriving stock (Show, Eq, Generic)

newtype EscapedChar = EscapedChar
  { _escapedChar :: Char
  }
  deriving stock (Show, Eq, Generic)

data Inline
  = InlineHardBreak (Meta HardBreak)
  | InlineSoftBreak (Meta SoftBreak)
  | InlineString (Meta Text)
  | InlineEntity (Meta Text)
  | InlineEscapedChar (Meta EscapedChar)
  | InlineEmph Inlines
  | InlineStrong Inlines
  | InlineLink (Meta Link)
  | InlineImage (Meta Image)
  | InlineCode (Meta Text)
  | InlineRaw (Meta RawInline)
  deriving stock (Show, Eq, Generic)

data CodeBlock = CodeBlock
  { _codeBlockLanguage :: Text,
    _codeBlock :: Text
  }
  deriving stock (Show, Eq, Generic)

data Heading = Heading
  { _headingLevel :: Int,
    _headingText :: Inlines
  }
  deriving stock (Show, Eq, Generic)

data RawBlock = RawBlock
  { _rawBlockFormat :: Format,
    _rawBlockText :: Text
  }
  deriving stock (Show, Eq, Generic)

data ReferenceLinkDefinition = ReferenceLinkDefinition
  { _referenceLinkDefinitionLabel :: Text,
    _referenceLinkDefinitionDestination :: Text,
    _referenceLinkDefinitionTitle :: Text
  }
  deriving stock (Show, Eq, Generic)

data List = List
  { _listType :: ListType,
    _listSpacing :: ListSpacing,
    _listBlocks :: NonEmpty Blocks
  }
  deriving stock (Show, Eq, Generic)

data ThematicBreak = ThematicBreak
  deriving stock (Show, Eq, Generic)

newtype QuoteBlock = QuoteBlock
  { _quoteBlock :: Blocks
  }
  deriving stock (Show, Eq, Generic)

data Block
  = BlockParagraph (Meta Inlines)
  | BlockPlain (Meta Inlines)
  | BlockHeading (Meta Heading)
  | BlockThematicBreak (Meta ThematicBreak)
  | BlockQuote (Meta QuoteBlock)
  | BlockCodeBlock (Meta CodeBlock)
  | BlockRawBlock (Meta RawBlock)
  | BlockReferenceLinkDefinition (Meta ReferenceLinkDefinition)
  | BlockList (Meta List)
  deriving stock (Show, Eq, Generic)

newtype Blocks = Blocks
  { _blocks :: [Block]
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype (Semigroup, Monoid)

mkBlocks :: Block -> Blocks
mkBlocks = Blocks . pure

newtype Inlines = Inlines
  { _inlines :: [Inline]
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype (Semigroup, Monoid)

makeLenses ''Link
makeLenses ''Inline
makeLenses ''Inlines
makeLenses ''Blocks
makeLenses ''Image
makeLenses ''CodeBlock
makeLenses ''Meta

mkMeta :: a -> Meta a
mkMeta _metaArg =
  Meta
    { _metaArg,
      _metaLoc = Irrelevant iniRange,
      _metaAttributes = []
    }

instance HasAttributes (Meta a) where
  addAttributes attr = over metaAttributes (attr ++)

instance Rangeable (Meta a) where
  ranged = set (metaLoc . unIrrelevant)

instance HasAttributes Inline where
  addAttributes _attr = trace "todo"

-- TODO
instance Rangeable Inline where
  ranged d =
    \case
      InlineHardBreak a -> InlineHardBreak (ranged d a)
      InlineSoftBreak a -> InlineSoftBreak (ranged d a)
      InlineString a -> InlineString (ranged d a)
      InlineEntity a -> InlineEntity (ranged d a)
      InlineEscapedChar a -> InlineEscapedChar (ranged d a)
      InlineEmph a -> InlineEmph (ranged d a)
      InlineStrong a -> InlineStrong (ranged d a)
      InlineLink a -> InlineLink (ranged d a)
      InlineImage a -> InlineImage (ranged d a)
      InlineCode a -> InlineCode (ranged d a)
      InlineRaw a -> InlineRaw (ranged d a)

instance HasAttributes Blocks where
  addAttributes attr = over blocks (map (addAttributes attr))

instance HasAttributes Inlines where
  addAttributes attr = over inlines (map (addAttributes attr))

instance Rangeable Blocks where
  ranged d bs = trace ("rangeable blocks " <> show (length (bs ^. blocks)) <> " " <> show d) (over blocks (map (ranged d)) bs)

instance Rangeable Inlines where
  ranged d = trace ("rangeable inlines " <> show d) . over inlines (map (ranged d))

class IsInlines a where
  toInlines :: a -> Inlines

instance IsInlines Inlines where
  toInlines = id

instance IsInlines Inline where
  toInlines e =
    Inlines
      { _inlines = [e]
      }

placeHolderInterval :: Interval
placeHolderInterval = intervalFromFile $(mkAbsFile "/<temporary location>")

iniLoc :: Interval
iniLoc = placeHolderInterval

iniRange :: SourceRange
-- iniRange = impossibleError "The SourceRange should never be accessed. It should be replaced by 'ranged' during parsing"
iniRange = mempty

instance Rangeable Block where
  ranged r b =
    trace
      ("rangeable block: " <> show b)
      ( case b of
          BlockParagraph a -> BlockParagraph (ranged r a)
          BlockPlain a -> BlockPlain (ranged r a)
          BlockCodeBlock a -> BlockCodeBlock (ranged r a)
          BlockHeading a -> BlockHeading (ranged r a)
          BlockThematicBreak a -> BlockThematicBreak (ranged r a)
          BlockQuote a -> BlockQuote (ranged r a)
          BlockList a -> BlockList (ranged r a)
          BlockRawBlock a -> BlockRawBlock (ranged r a)
          BlockReferenceLinkDefinition a -> BlockReferenceLinkDefinition (ranged r a)
      )

instance HasAttributes Block where
  addAttributes _ = trace "attributes block"

instance IsInline Inlines where
  lineBreak = toInlines (InlineHardBreak (mkMeta HardBreak))
  softBreak = toInlines (InlineSoftBreak (mkMeta SoftBreak))
  str a = toInlines (InlineString (mkMeta a))
  entity a = toInlines (InlineEntity (mkMeta a))
  escapedChar _escapedChar = toInlines (InlineEscapedChar (mkMeta (EscapedChar {..})))
  emph a = toInlines (InlineEmph a)
  strong a = toInlines (InlineStrong a)
  link _linkDestination _linkTitle _linkDescription = toInlines (InlineLink (mkMeta Link {..}))
  image _imageSource _imageTitle _imageDescription = toInlines (InlineImage (mkMeta Image {..}))
  code a = toInlines (InlineCode (mkMeta a))
  rawInline _rawInlineFormat _rawInlineText = toInlines (InlineRaw (mkMeta RawInline {..}))

instance IsBlock Inlines Blocks where
  plain p = mkBlocks (BlockPlain (mkMeta p))
  paragraph i = mkBlocks (BlockParagraph (mkMeta i))
  thematicBreak = mkBlocks (BlockThematicBreak (mkMeta ThematicBreak))
  blockQuote _quoteBlock = mkBlocks (BlockQuote (mkMeta QuoteBlock {..}))
  codeBlock _codeBlockLanguage _codeBlock = mkBlocks (BlockCodeBlock (mkMeta (CodeBlock {..})))
  heading _headingLevel _headingText = mkBlocks (BlockHeading (mkMeta Heading {..}))
  rawBlock = error "todo"
  referenceLinkDefinition _referenceLinkDefinitionLabel (_referenceLinkDefinitionDestination, _referenceLinkDefinitionTitle) = mkBlocks (BlockReferenceLinkDefinition (mkMeta ReferenceLinkDefinition {..}))
  list _listType _listSpacing lstBlocks = mkBlocks (BlockList (mkMeta List {_listBlocks = nonEmpty' lstBlocks, ..}))

instance (Pretty a) => Pretty (Meta a) where
  pretty = pretty . (^. metaArg)
