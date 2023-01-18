module Juvix.Data.Comment where

import Juvix.Data.Loc
import Juvix.Prelude.Base
import Path
import Prettyprinter

newtype Comments = Comments
  { _commentsByFile :: HashMap (Path Abs File) (NonEmpty FileComments)
  }
  deriving stock (Eq, Show, Generic, Data)

data FileComments = FileComments
  { -- | sorted by position
    _fileCommentsSorted :: NonEmpty Comment,
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
        CommentOneLine -> (<> "--")
        CommentBlock -> enclose "{-" "-}"

mkComments :: [Comment] -> Comments
mkComments = undefined
