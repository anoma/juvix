module Juvix.Data.Comment where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Data.Loc
import Juvix.Prelude.Base
import Juvix.Prelude.Lens
import Path
import Prettyprinter

newtype Comments = Comments
  { _commentsByFile :: HashMap (Path Abs File) FileComments
  }
  deriving stock (Eq, Show, Generic, Data)

data FileComments = FileComments
  { -- | sorted by position
    _fileCommentsSorted :: [CommentGroup],
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

-- | A sequence of consecutive comments not separated by one or more empty
-- lines.
newtype CommentGroup = CommentGroup
  { _commentGroup :: NonEmpty Comment
  }
  deriving stock (Show, Eq, Ord, Generic, Data)

deriving newtype instance Semigroup CommentGroup

makeLenses ''Comment
makeLenses ''CommentGroup
makeLenses ''FileComments
makeLenses ''Comments

instance HasLoc CommentGroup where
  getLoc = getLocSpan . (^. commentGroup)

instance HasLoc Comment where
  getLoc = (^. commentInterval)

instance Pretty CommentGroup where
  pretty :: CommentGroup -> Doc ann
  pretty = vsep . map pretty . toList . (^. commentGroup)

instance Pretty Comment where
  pretty :: Comment -> Doc ann
  pretty c = delim (pretty (c ^. commentText))
    where
      delim :: Doc ann -> Doc ann
      delim = case c ^. commentType of
        CommentOneLine -> ("--" <>)
        CommentBlock -> enclose "{-" "-}"

flattenComments :: [CommentGroup] -> [Comment]
flattenComments = mconcatMap (^. commentGroup . to toList)

allComments :: Comments -> [CommentGroup]
allComments c = concat [f ^. fileCommentsSorted | f <- toList (c ^. commentsByFile)]

mkComments :: [CommentGroup] -> Comments
mkComments cs = Comments {..}
  where
    commentFile :: CommentGroup -> Path Abs File
    commentFile = (^. commentGroup . _head1 . commentInterval . intervalFile)
    _commentsByFile :: HashMap (Path Abs File) FileComments
    _commentsByFile =
      HashMap.fromList
        [ (_fileCommentsFile, FileComments {..})
          | filecomments :: NonEmpty CommentGroup <- groupSortOn commentFile cs,
            let _fileCommentsFile = commentFile (head filecomments),
            let _fileCommentsSorted = sortOn (^. commentGroup . _head1 . commentInterval) (toList filecomments)
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

instance Pretty FileComments where
  pretty fc =
    pretty (fc ^. fileCommentsFile)
      <> line
      <> vsep [pretty c | c <- toList (fc ^. fileCommentsSorted)]

instance Pretty Comments where
  pretty c
    | null (c ^. commentsByFile) = "<empty comments>"
    | otherwise = vsep [pretty fc | fc <- toList (c ^. commentsByFile)]
