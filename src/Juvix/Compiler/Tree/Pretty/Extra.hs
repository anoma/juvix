module Juvix.Compiler.Tree.Pretty.Extra where

import Data.Text qualified as Text
import Juvix.Data.CodeAnn
import Juvix.Prelude

braces' :: Doc Ann -> Doc Ann
braces' d = braces (line <> indent' d <> line)

integer :: (Pretty a) => a -> Doc Ann
integer i = annotate AnnLiteralInteger (pretty i)

constr :: Text -> Doc Ann
constr a = annotate (AnnKind KNameConstructor) (pretty a)

variable :: Text -> Doc Ann
variable a = annotate (AnnKind KNameLocal) (pretty a)

quoteName :: Text -> Text
quoteName txt =
  foldr
    (uncurry Text.replace)
    txt
    [ ("$", "__dollar__"),
      (":", "__colon__"),
      ("@", "__at__"),
      (".", "__dot__"),
      (",", "__comma__"),
      (";", "__semicolon__"),
      ("arg", "__arg__"),
      ("tmp", "__tmp__"),
      ("sub", "__sub__"),
      ("add", "__add__"),
      ("mul", "__mul__"),
      ("div", "__div__")
    ]

quoteFunName :: Text -> Text
quoteFunName txt =
  foldr
    (uncurry Text.replace)
    txt
    [ ("readLn", "__readLn__")
    ]
