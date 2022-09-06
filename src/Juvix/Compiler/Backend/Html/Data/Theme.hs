module Juvix.Compiler.Backend.Html.Data.Theme where

import Juvix.Prelude

data Theme
  = Nord
  | Ayu
  deriving stock (Show, Enum, Bounded)

data HtmlKind
  = HtmlDoc
  | HtmlSrc
  | HtmlOnly
