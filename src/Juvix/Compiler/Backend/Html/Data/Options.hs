module Juvix.Compiler.Backend.Html.Data.Options where

import Juvix.Prelude

data HtmlOptions = HtmlOptions
  { _htmlOptionsKind :: HtmlKind,
    _htmlOptionsAssetsDir :: Text, -- TODO: can we have a path here.
    _htmlOptionsPrefixUrl :: Text,
    _htmlOptionsOutputDir :: Path Abs Dir,
    _htmlOptionsParamBase :: Text
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
