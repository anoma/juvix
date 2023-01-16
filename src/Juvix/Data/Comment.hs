module Juvix.Data.Comment where

import Juvix.Data.Loc
import Juvix.Prelude.Base
import Prettyprinter

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

instance Pretty Comment where
  pretty :: Comment -> Doc ann
  pretty c = delim (pretty (c ^. commentText))
    where
      delim :: Doc ann -> Doc ann
      delim = case c ^. commentType of
        CommentOneLine -> (<> "--")
        CommentBlock -> enclose "{-" "-}"
