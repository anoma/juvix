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

data Link = Link
  { _linkDestination :: Text,
    _linkTitle :: Text,
    _linkDescription :: Inlines
  }
  deriving stock (Show)

data Image = Image
  { _imageSource :: Text,
    _imageTitle :: Text,
    _imageDescription :: Inlines
  }
  deriving stock (Show)

data RawInline = RawInline
  { _rawInlineFormat :: Format,
    _rawInlineText :: Text
  }
  deriving stock (Show)

data InlineElem
  = InlineLineBreak
  | InlineSoftBreak
  | InlineString Text
  | InlineEntity Text
  | InlineEscapedChar Char
  | InlineEmph Inlines
  | InlineStrong Inlines
  | InlineLink Link
  | InlineImage Image
  | InlineCode Text
  | InlineRaw RawInline
  deriving stock (Show)

data CodeBlock = CodeBlock
  { _codeBlockLanguage :: Text,
    _codeBlock :: Text
  }
  deriving stock (Show)

data Heading = Heading
  { _headingLabel :: Text,
    _headingText :: Inline
  }
  deriving stock (Show)

data RawBlock = RawBlock
  { _rawBlockFormat :: Format,
    _rawBlockText :: Text
  }
  deriving stock (Show)

data ReferenceLinkDefinition = ReferenceLinkDefinition
  { _referenceLinkDefinitionLabel :: Text,
    _referenceLinkDefinitionDestination :: Text,
    _referenceLinkDefinitionTitle :: Text
  }
  deriving stock (Show)

data List = List
  { _listType :: ListType,
    _listSpacing :: ListSpacing,
    _listBlocks :: [Block]
  }
  deriving stock (Show)

data Block
  = BlockParagraph Inline
  | BlockPlain Inline
  | BlockThematicBreak Block
  | BlockQuote Block
  | BlockCodeBlock CodeBlock
  | BlockRawBlock RawBlock
  | BlockReferenceLinkDefinition ReferenceLinkDefinition
  | BlockList List
  deriving stock (Show)

data Inline = Inline
  { _inlineAttributes :: [Attribute],
    _inlineElem :: InlineElem
  }
  deriving stock (Show)

newtype Inlines = Inlines
  { _inlines :: [Inline]
  }
  deriving stock (Show)
  deriving newtype (Semigroup, Monoid)

makeLenses ''Link
makeLenses ''Inline
makeLenses ''Inlines
makeLenses ''Image
makeLenses ''CodeBlock

instance HasAttributes Inline where
  addAttributes attr = (over inlineAttributes (attr ++))

-- TODO
instance Rangeable Inline where
  ranged _ = error "todo"

instance HasAttributes Inlines where
  addAttributes attr = over inlines (map (addAttributes attr))

-- TODO
instance Rangeable Inlines where
  ranged _ = error "todo"

class IsInlines a where
  toInlines :: a -> Inlines

instance IsInlines Inlines where
  toInlines = id

instance IsInlines InlineElem where
  toInlines e =
    Inlines
      { _inlines =
          [ Inline
              { _inlineAttributes = [],
                _inlineElem = e
              }
          ]
      }

instance IsInline Inlines where
  lineBreak = toInlines InlineLineBreak
  softBreak = toInlines InlineSoftBreak
  str a = toInlines (InlineString a)
  entity a = toInlines (InlineEntity a)
  escapedChar a = toInlines (InlineEscapedChar a)
  emph a = toInlines (InlineEmph a)
  strong a = toInlines (InlineStrong a)
  link _linkDestination _linkTitle _linkDescription = toInlines (InlineLink Link {..})
  image _imageSource _imageTitle _imageDescription = toInlines (InlineImage Image {..})
  code a = toInlines (InlineCode a)
  rawInline _rawInlineFormat _rawInlineText = toInlines (InlineRaw (RawInline {..}))
