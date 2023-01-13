module Juvix.Compiler.Backend.Html.Data.Options where

import Juvix.Prelude

data PlainHtmlOptions = PlainHtmlOptions
  { _htmlOptionsKind :: HtmlKind,
    _htmlOptionsBaseUrl :: Text
  }

data Theme
  = Nord
  | Ayu
  deriving stock (Show, Enum, Bounded, Data)

data HtmlKind
  = HtmlDoc
  | HtmlSrc
  | HtmlOnly
  deriving stock (Data)

makeLenses ''PlainHtmlOptions
