module Juvix.Compiler.Backend.Html.Data.Theme where

import Juvix.Prelude

data Theme
  = Nord
  | Ayu
  deriving stock (Show)

data HtmlKind
  = HtmlDoc
  | HtmlSrc
  | HtmlOnly
