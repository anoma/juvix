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

newtype Strong = Strong
  { _strong :: Inlines
  }
  deriving stock (Show, Eq, Generic)

newtype Emph = Emph
  { _emph :: Inlines
  }
  deriving stock (Show, Eq, Generic)

newtype Code = Code
  { _code :: Text
  }
  deriving stock (Show, Eq, Generic)

newtype Entity = Entity
  { _entity :: Text
  }
  deriving stock (Show, Eq, Generic)

data Inline
  = InlineHardBreak (Meta HardBreak)
  | InlineSoftBreak (Meta SoftBreak)
  | InlineString (Meta Text)
  | InlineEntity (Meta Entity)
  | InlineEscapedChar (Meta EscapedChar)
  | InlineEmph (Meta Emph)
  | InlineStrong (Meta Strong)
  | InlineLink (Meta Link)
  | InlineImage (Meta Image)
  | InlineCode (Meta Code)
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
  addAttributes attr =
    \case
      InlineHardBreak a -> InlineHardBreak (addAttributes attr a)
      InlineSoftBreak a -> InlineSoftBreak (addAttributes attr a)
      InlineString a -> InlineString (addAttributes attr a)
      InlineEntity a -> InlineEntity (addAttributes attr a)
      InlineEscapedChar a -> InlineEscapedChar (addAttributes attr a)
      InlineEmph a -> InlineEmph (addAttributes attr a)
      InlineStrong a -> InlineStrong (addAttributes attr a)
      InlineLink a -> InlineLink (addAttributes attr a)
      InlineImage a -> InlineImage (addAttributes attr a)
      InlineCode a -> InlineCode (addAttributes attr a)
      InlineRaw a -> InlineRaw (addAttributes attr a)

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
  ranged d bs = over blocks (map (ranged d)) bs

instance Rangeable Inlines where
  ranged d = over inlines (map (ranged d))

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
  ranged r = \case
    BlockParagraph a -> BlockParagraph (ranged r a)
    BlockPlain a -> BlockPlain (ranged r a)
    BlockCodeBlock a -> BlockCodeBlock (ranged r a)
    BlockHeading a -> BlockHeading (ranged r a)
    BlockThematicBreak a -> BlockThematicBreak (ranged r a)
    BlockQuote a -> BlockQuote (ranged r a)
    BlockList a -> BlockList (ranged r a)
    BlockRawBlock a -> BlockRawBlock (ranged r a)
    BlockReferenceLinkDefinition a -> BlockReferenceLinkDefinition (ranged r a)

instance HasAttributes Block where
  addAttributes attr = \case
    BlockParagraph a -> BlockParagraph (addAttributes attr a)
    BlockPlain a -> BlockPlain (addAttributes attr a)
    BlockCodeBlock a -> BlockCodeBlock (addAttributes attr a)
    BlockHeading a -> BlockHeading (addAttributes attr a)
    BlockThematicBreak a -> BlockThematicBreak (addAttributes attr a)
    BlockQuote a -> BlockQuote (addAttributes attr a)
    BlockList a -> BlockList (addAttributes attr a)
    BlockRawBlock a -> BlockRawBlock (addAttributes attr a)
    BlockReferenceLinkDefinition a -> BlockReferenceLinkDefinition (addAttributes attr a)

instance IsInline Inlines where
  lineBreak = toInlines (InlineHardBreak (mkMeta HardBreak))
  softBreak = toInlines (InlineSoftBreak (mkMeta SoftBreak))
  str a = toInlines (InlineString (mkMeta a))
  entity a = toInlines (InlineEntity (mkMeta (Entity a)))
  escapedChar _escapedChar = toInlines (InlineEscapedChar (mkMeta (EscapedChar {..})))
  emph a = toInlines (InlineEmph (mkMeta (Emph a)))
  strong a = toInlines (InlineStrong (mkMeta (Strong a)))
  link _linkDestination _linkTitle _linkDescription = toInlines (InlineLink (mkMeta Link {..}))
  image _imageSource _imageTitle _imageDescription = toInlines (InlineImage (mkMeta Image {..}))
  code a = toInlines (InlineCode (mkMeta (Code a)))
  rawInline _rawInlineFormat _rawInlineText = toInlines (InlineRaw (mkMeta RawInline {..}))

instance IsBlock Inlines Blocks where
  plain p = mkBlocks (BlockPlain (mkMeta p))
  paragraph i = mkBlocks (BlockParagraph (mkMeta i))
  thematicBreak = mkBlocks (BlockThematicBreak (mkMeta ThematicBreak))
  blockQuote _quoteBlock = mkBlocks (BlockQuote (mkMeta QuoteBlock {..}))
  codeBlock _codeBlockLanguage _codeBlock = mkBlocks (BlockCodeBlock (mkMeta (CodeBlock {..})))
  heading _headingLevel _headingText = mkBlocks (BlockHeading (mkMeta Heading {..}))
  rawBlock _rawBlockFormat _rawBlockText = mkBlocks (BlockRawBlock (mkMeta RawBlock {..}))
  referenceLinkDefinition _referenceLinkDefinitionLabel (_referenceLinkDefinitionDestination, _referenceLinkDefinitionTitle) = mkBlocks (BlockReferenceLinkDefinition (mkMeta ReferenceLinkDefinition {..}))
  list _listType _listSpacing lstBlocks = mkBlocks (BlockList (mkMeta List {_listBlocks = nonEmpty' lstBlocks, ..}))

instance (Pretty a) => Pretty (Meta a) where
  pretty = pretty . (^. metaArg)
