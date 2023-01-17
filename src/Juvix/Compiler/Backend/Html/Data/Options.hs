module Juvix.Compiler.Backend.Html.Data.Options where

import Juvix.Prelude

data HtmlOptions = HtmlOptions
  { _htmlOptionsKind :: HtmlKind,
    _htmlOptionsAssetsPrefix :: Text,
    _htmlOptionsUrlPrefix :: Text,
    _htmlOptionsOutputDir :: Path Abs Dir,
    _htmlOptionsParamBase :: Text,
    _htmlOptionsTheme :: Theme,
    _htmlOptionsNoFooter :: Bool
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

makeLenses ''HtmlOptions
