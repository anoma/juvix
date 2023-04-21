module Juvix.Data.Comment where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Data.Loc
import Juvix.Prelude.Base
import Path
import Prettyprinter

newtype Comments = Comments
  { _commentsByFile :: HashMap (Path Abs File) FileComments
  }
  deriving stock (Eq, Show, Generic, Data)

data FileComments = FileComments
  { -- | sorted by position
    _fileCommentsSorted :: [SpaceSpan],
    _fileCommentsFile :: Path Abs File
  }
  deriving stock (Eq, Show, Generic, Data)

data CommentType
  = CommentOneLine
  | CommentBlock
  deriving stock (Eq, Ord, Show, Generic, Data)

data Comment = Comment
  { _commentType :: CommentType,
    _commentText :: Text,
    _commentInterval :: Interval
  }
  deriving stock (Show, Eq, Ord, Generic, Data)

data SpaceSection
  = SpaceComment Comment
  | SpaceLines EmptyLines
  deriving stock (Show, Eq, Ord, Generic, Data)

-- | One or more empty lines
data EmptyLines = EmptyLines
  { _emptyLinesLoc :: Interval,
    -- | The number of empty lines. Always positive
    _emptyLinesNum :: Int
  }
  deriving stock (Show, Eq, Ord, Generic, Data)

newtype SpaceSpan = SpaceSpan
  { _spaceSpan :: NonEmpty SpaceSection
  }
  deriving stock (Show, Eq, Ord, Generic, Data)

deriving newtype instance Semigroup SpaceSpan

makeLenses ''Comment
makeLenses ''SpaceSpan
makeLenses ''FileComments
makeLenses ''Comments
makeLenses ''EmptyLines

_SpaceLines :: Traversal' SpaceSection EmptyLines
_SpaceLines f s = case s of
  SpaceComment {} -> pure s
  SpaceLines l -> SpaceLines <$> f l

hasEmptyLines :: SpaceSpan -> Bool
hasEmptyLines = any (has _SpaceLines) . (^. spaceSpan)

instance HasLoc SpaceSpan where
  getLoc = getLocSpan . (^. spaceSpan)

instance HasLoc EmptyLines where
  getLoc = (^. emptyLinesLoc)

instance HasLoc SpaceSection where
  getLoc = \case
    SpaceComment g -> getLoc g
    SpaceLines w -> getLoc w

instance HasLoc Comment where
  getLoc = (^. commentInterval)

instance Pretty Comment where
  pretty :: Comment -> Doc ann
  pretty c = delim (pretty (c ^. commentText))
    where
      delim :: Doc ann -> Doc ann
      delim = case c ^. commentType of
        CommentOneLine -> ("--" <>)
        CommentBlock -> enclose "{-" "-}"

allComments :: Comments -> [Comment]
allComments c =
  [ m | f <- toList (c ^. commentsByFile), s <- f ^. fileCommentsSorted, SpaceComment m <- toList (s ^. spaceSpan)
  ]

mkComments :: [SpaceSpan] -> Comments
mkComments cs = Comments {..}
  where
    spSpanFile :: SpaceSpan -> Path Abs File
    spSpanFile = (^. intervalFile) . getLoc
    _commentsByFile :: HashMap (Path Abs File) FileComments
    _commentsByFile =
      HashMap.fromList
        [ (_fileCommentsFile, FileComments {..})
          | filecomments :: NonEmpty SpaceSpan <- groupSortOn spSpanFile cs,
            let _fileCommentsFile = spSpanFile (head filecomments),
            let _fileCommentsSorted = sortOn getLoc (toList filecomments)
        ]

emptyComments :: Comments
emptyComments = Comments mempty

emptyFileComments :: Path Abs File -> FileComments
emptyFileComments _fileCommentsFile =
  FileComments
    { _fileCommentsSorted = [],
      ..
    }

fileComments :: Path Abs File -> Comments -> FileComments
fileComments f cs = HashMap.lookupDefault (emptyFileComments f) f (cs ^. commentsByFile)

flattenComments :: [SpaceSpan] -> [Comment]
flattenComments m = [c | s <- m, SpaceComment c <- toList (s ^. spaceSpan)]

instance Pretty FileComments where
  pretty fc =
    pretty (fc ^. fileCommentsFile)
      <> line
      <> vsep [pretty c | c <- flattenComments (fc ^. fileCommentsSorted)]

instance Pretty Comments where
  pretty c
    | null (c ^. commentsByFile) = "<empty comments>"
    | otherwise = vsep [pretty fc | fc <- toList (c ^. commentsByFile)]
