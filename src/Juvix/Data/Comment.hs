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
    _fileCommentsSorted :: [Comment],
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

makeLenses ''Comment
makeLenses ''FileComments
makeLenses ''Comments

instance Pretty Comment where
  pretty :: Comment -> Doc ann
  pretty c = delim (pretty (c ^. commentText))
    where
      delim :: Doc ann -> Doc ann
      delim = case c ^. commentType of
        CommentOneLine -> ("--" <>)
        CommentBlock -> enclose "{-" "-}"

allComments :: Comments -> [Comment]
allComments c = concat [f ^. fileCommentsSorted | f <- toList (c ^. commentsByFile)]

mkComments :: [Comment] -> Comments
mkComments cs = Comments {..}
  where
    commentFile :: Comment -> Path Abs File
    commentFile = (^. commentInterval . intervalFile)
    _commentsByFile :: HashMap (Path Abs File) FileComments
    _commentsByFile =
      HashMap.fromList
        [ (_fileCommentsFile, FileComments {..})
          | filecomments :: NonEmpty Comment <- groupSortOn commentFile cs,
            let _fileCommentsFile = commentFile (head filecomments),
            let _fileCommentsSorted = sortOn (^. commentInterval) (toList filecomments)
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
  pretty c = vsep [pretty fc | fc <- toList (c ^. commentsByFile)]
