module Markdown.Language
  ( module Markdown.Language,
    Attribute,
    Format,
    ListSpacing,
    ListType,
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

data Inline
  = InlineLineBreak
  | InlineSoftBreak
  | InlineString (Meta Text)
  | InlineEntity Text
  | InlineEscapedChar Char
  | InlineEmph Inlines
  | InlineStrong Inlines
  | InlineLink Link
  | InlineImage Image
  | InlineCode Text
  | InlineRaw RawInline
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
    _listBlocks :: [Block]
  }
  deriving stock (Show, Eq, Generic)

data Block
  = BlockParagraph (Meta Inlines)
  | BlockPlain (Meta Inlines)
  | BlockHeading (Meta Heading)
  | BlockThematicBreak Block
  | BlockQuote Block
  | BlockCodeBlock (Meta CodeBlock)
  | BlockRawBlock RawBlock
  | BlockReferenceLinkDefinition ReferenceLinkDefinition
  | BlockList List
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
  addAttributes _attr = error "todo"

-- TODO
instance Rangeable Inline where
  ranged d =
    \case
      InlineString s -> InlineString (ranged d s)
      x ->
        trace
          ("* rangeable inline: '" <> (show x) <> "' " <> show d)
          x

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
          x -> error ("TODO: " <> show x)
      )

instance HasAttributes Block where
  addAttributes _ = error "todo"

instance IsInline Inlines where
  lineBreak = toInlines InlineLineBreak
  softBreak = toInlines InlineSoftBreak
  str a = toInlines (InlineString (mkMeta a))
  entity a = toInlines (InlineEntity a)
  escapedChar a = toInlines (InlineEscapedChar a)
  emph a = toInlines (InlineEmph a)
  strong a = toInlines (InlineStrong a)
  link _linkDestination _linkTitle _linkDescription = toInlines (InlineLink Link {..})
  image _imageSource _imageTitle _imageDescription = toInlines (InlineImage Image {..})
  code a = toInlines (InlineCode a)
  rawInline _rawInlineFormat _rawInlineText = toInlines (InlineRaw (RawInline {..}))

instance IsBlock Inlines Blocks where
  plain p = mkBlocks (BlockPlain (mkMeta p))
  paragraph i = mkBlocks (BlockParagraph (mkMeta i))
  thematicBreak = error "todo"
  blockQuote = error "todo"
  codeBlock _codeBlockLanguage _codeBlock = mkBlocks (BlockCodeBlock (mkMeta (CodeBlock {..})))
  heading _headingLevel _headingText = mkBlocks (BlockHeading (mkMeta Heading {..}))
  rawBlock = error "todo"
  referenceLinkDefinition = error "todo"
  list = error "todo"

instance (Pretty a) => Pretty (Meta a) where
  pretty = pretty . (^. metaArg)
