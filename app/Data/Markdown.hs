module Data.Markdown where

import Commonmark.Types
import Juvix.Prelude hiding (Raw)

data LineBreak = LineBreak
  { _lineBreakAttr :: [Attribute],
    _lineBreakSourceRange :: SourceRange
  }
  deriving stock (Show)

data SoftBreak = SoftBreak
  { _softBreakAttr :: [Attribute],
    _softBreakSourceRange :: SourceRange
  }
  deriving stock (Show)

data Str = Str
  { _strAttr :: [Attribute],
    _strSourceRange :: SourceRange,
    _strText :: Text
  }
  deriving stock (Show)

data Entity = Entity
  { _entityAttr :: [Attribute],
    _entitySourceRange :: SourceRange,
    _entityText :: Text
  }
  deriving stock (Show)

data EscapedChar = EscapedChar
  { _escapedCharAttr :: [Attribute],
    _escapedCharSourceRange :: SourceRange,
    _escapedChar :: Char
  }
  deriving stock (Show)

data Emph = Emph
  { _emphAttr :: [Attribute],
    _emphSourceRange :: SourceRange,
    _emphText :: Inlines
  }
  deriving stock (Show)

data Strong = Strong
  { _strongAttr :: [Attribute],
    _strongSourceRange :: SourceRange,
    _strongInlines :: Inlines
  }
  deriving stock (Show)

data Link = Link
  { _linkAttr :: [Attribute],
    _linkSourceRange :: SourceRange,
    _linkDestination :: Text,
    _linkTitle :: Text,
    _linkDescription :: Inlines
  }
  deriving stock (Show)

data Image = Image
  { _imageAttr :: [Attribute],
    _imageSourceRange :: SourceRange,
    _imageSource :: Text,
    _imageTitle :: Text,
    _imageDescription :: Inlines
  }
  deriving stock (Show)

data Code = Code
  { _codeAttr :: [Attribute],
    _codeSourceRange :: SourceRange,
    _codeText :: Text
  }
  deriving stock (Show)

data Raw = Raw
  { _rawAttr :: [Attribute],
    _rawSourceRange :: SourceRange,
    _rawFormat :: Format,
    _rawText :: Text
  }
  deriving stock (Show)

data Inline
  = InlineLineBreak LineBreak
  | InlineSoftBreak SoftBreak
  | InlineStr Str
  | InlineEntity Entity
  | InlineEscapedChar EscapedChar
  | InlineEmph Emph
  | InlineStrong Strong
  | InlineLink Link
  | InlineImage Image
  | InlineCode Code
  | InlineRaw Raw
  deriving stock (Show)

data Inlines = Inlines
  { _inlinesInlines :: [Inline],
    _inlinesSourceRange :: SourceRange
  }
  deriving stock (Show)

data Paragraph = Paragraph
   { _paragraphAttr :: [Attribute],
   _paragraphInline :: Inlines
  }
  deriving stock (Show)

data Plain = Plain
   { _plainAttr :: [Attribute],
   _plainInline :: Inlines
  }
  deriving stock (Show)

data Heading = Heading
  {
    _headingAttr :: [Attribute],
    _headingLevel :: Int,
    _headingInline :: Inlines
  }
  deriving stock (Show)

data BCode = BCode
  {
    _bcodeAttr :: [Attribute],
    _bcodeSourceRange :: SourceRange,
    _bcodeInfo :: Text,
    _bcodeCode :: Text
  }
  deriving stock (Show)

data Block
  = BlockParagraph Paragraph
  | BlockPlain Plain
  | BlockHeading Heading
  | BlockCode BCode
  deriving stock (Show)

data Blocks = Blocks
  { _blocksBlocks :: [Block],
    _blocksSourceRange :: SourceRange
  }
  deriving stock (Show)

makeLenses ''LineBreak
makeLenses ''SoftBreak
makeLenses ''Str
makeLenses ''Entity
makeLenses ''EscapedChar
makeLenses ''Emph
makeLenses ''Strong
makeLenses ''Link
makeLenses ''Image
makeLenses ''Code
makeLenses ''Raw
makeLenses ''Inlines
makeLenses ''Heading
makeLenses ''Blocks
makeLenses ''Paragraph
makeLenses ''BCode
makeLenses ''Plain

instance Semigroup Blocks where
  a <> b =
    Blocks
      { _blocksBlocks = a ^. blocksBlocks <> b ^. blocksBlocks,
        _blocksSourceRange = a ^. blocksSourceRange <> b ^. blocksSourceRange
      }

instance Monoid Blocks where
  mempty =
    Blocks
      { _blocksBlocks = mempty,
        _blocksSourceRange = mempty
      }

instance HasAttributes LineBreak where
  addAttributes a = over lineBreakAttr (<> a)

instance HasAttributes SoftBreak where
  addAttributes a = over softBreakAttr (<> a)

instance HasAttributes Str where
  addAttributes a = over strAttr (<> a)

instance HasAttributes Entity where
  addAttributes a = over entityAttr (<> a)

instance HasAttributes EscapedChar where
  addAttributes a = over escapedCharAttr (<> a)

instance HasAttributes Emph where
  addAttributes a = over emphAttr (<> a)

instance HasAttributes Strong where
  addAttributes a = over strongAttr (<> a)

instance HasAttributes Link where
  addAttributes a = over linkAttr (<> a)

instance HasAttributes Image where
  addAttributes a = over imageAttr (<> a)

instance HasAttributes Code where
  addAttributes a = over codeAttr (<> a)

instance HasAttributes Raw where
  addAttributes a = over rawAttr (<> a)

instance Rangeable LineBreak where
  ranged r = set lineBreakSourceRange r

instance Rangeable SoftBreak where
  ranged r = set softBreakSourceRange r

instance Rangeable Str where
  ranged r = set strSourceRange r

instance Rangeable Entity where
  ranged r = set entitySourceRange r

instance Rangeable EscapedChar where
  ranged r = set escapedCharSourceRange r

instance Rangeable Emph where
  ranged r = set emphSourceRange r

instance Rangeable Strong where
  ranged r = set strongSourceRange r

instance Rangeable Link where
  ranged r = set linkSourceRange r

instance Rangeable Image where
  ranged r = set imageSourceRange r

instance Rangeable Code where
  ranged r = set codeSourceRange r

instance Rangeable Raw where
  ranged r = set rawSourceRange r

instance HasAttributes Inline where
  addAttributes a = \case
    InlineLineBreak l -> InlineLineBreak (addAttributes a l)
    InlineSoftBreak l -> InlineSoftBreak (addAttributes a l)
    InlineStr l -> InlineStr (addAttributes a l)
    InlineEntity l -> InlineEntity (addAttributes a l)
    InlineEscapedChar l -> InlineEscapedChar (addAttributes a l)
    InlineEmph l -> InlineEmph (addAttributes a l)
    InlineStrong l -> InlineStrong (addAttributes a l)
    InlineLink l -> InlineLink (addAttributes a l)
    InlineImage l -> InlineImage (addAttributes a l)
    InlineCode l -> InlineCode (addAttributes a l)
    InlineRaw l -> InlineRaw (addAttributes a l)

instance HasAttributes Inlines where
  addAttributes a = over (inlinesInlines . _last) (addAttributes a)

instance Rangeable Inlines where
  ranged = set inlinesSourceRange

instance Semigroup Inlines where
  a <> b =
    Inlines
      { _inlinesInlines = a ^. inlinesInlines <> b ^. inlinesInlines,
        _inlinesSourceRange = a ^. inlinesSourceRange <> b ^. inlinesSourceRange
      }

instance Monoid Inlines where
  mempty =
    Inlines
      { _inlinesInlines = mempty,
        _inlinesSourceRange = mempty
      }

inlineSourceRange :: Inline -> SourceRange
inlineSourceRange = \case
  InlineLineBreak l -> l ^. lineBreakSourceRange
  InlineSoftBreak l -> l ^. softBreakSourceRange
  InlineStr l -> l ^. strSourceRange
  InlineEntity l -> l ^. entitySourceRange
  InlineEscapedChar l -> l ^. escapedCharSourceRange
  InlineEmph l -> l ^. emphSourceRange
  InlineStrong l -> l ^. strongSourceRange
  InlineLink l -> l ^. linkSourceRange
  InlineImage l -> l ^. imageSourceRange
  InlineCode l -> l ^. codeSourceRange
  InlineRaw l -> l ^. rawSourceRange

s :: Inline -> Inlines
s i =
  Inlines
    { _inlinesInlines = [i],
      _inlinesSourceRange = inlineSourceRange i
    }

instance IsInline Inlines where
  lineBreak :: Inlines
  lineBreak = s (InlineLineBreak (LineBreak mempty mempty))
  softBreak = s (InlineSoftBreak (SoftBreak mempty mempty))
  str txt = s (InlineStr (Str mempty mempty txt))
  entity txt = s (InlineEntity (Entity mempty mempty txt))
  escapedChar ch = s (InlineEscapedChar (EscapedChar mempty mempty ch))
  emph :: Inlines -> Inlines
  emph txt = s (InlineEmph (Emph mempty mempty txt))
  strong txt = s (InlineStrong (Strong mempty mempty txt))
  link dest titl i = s (InlineLink (Link mempty mempty dest titl i))
  image src titl desc = s (InlineImage (Image mempty mempty src titl desc))
  code txt = s (InlineCode (Code mempty mempty txt))
  rawInline f txt = s (InlineRaw (Raw mempty mempty f txt))

instance HasAttributes Paragraph where
  addAttributes a = over paragraphAttr (<> a)

instance HasAttributes Plain where
  addAttributes a = over plainAttr (<> a)

instance HasAttributes Heading where
  addAttributes a = over headingAttr (<> a)

instance HasAttributes BCode where
  addAttributes a = over bcodeAttr (<> a)

instance HasAttributes Block where
  addAttributes a = \case
    BlockParagraph p -> BlockParagraph (addAttributes a p)
    BlockPlain p -> BlockPlain (addAttributes a p)
    BlockHeading p -> BlockHeading (addAttributes a p)
    BlockCode p -> BlockCode (addAttributes a p)

instance HasAttributes Blocks where
  addAttributes a = over (blocksBlocks . _last) (addAttributes a)

instance Rangeable Blocks where
  ranged = set blocksSourceRange

blockSourceRange :: Block -> SourceRange
blockSourceRange = \case
  BlockParagraph p -> p ^. paragraphInline . inlinesSourceRange
  BlockPlain p -> p ^. plainInline . inlinesSourceRange
  BlockHeading p -> p ^. headingInline . inlinesSourceRange
  BlockCode p -> p ^. bcodeSourceRange

bs :: Block -> Blocks
bs i =
  Blocks
    { _blocksBlocks = [i],
      _blocksSourceRange = blockSourceRange i
    }

instance IsBlock Inlines Blocks where
  paragraph i = bs (BlockParagraph (Paragraph mempty i))
  plain i = bs (BlockPlain (Plain mempty i))
  heading lvl i = bs (BlockHeading (Heading mempty lvl i))
  codeBlock info txt = bs (BlockCode (BCode mempty mempty info txt))
