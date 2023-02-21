module Juvix.Compiler.Backend.Html.Data.Options
  ( module Juvix.Compiler.Backend.Html.Data.Options,
    module Juvix.Data.HtmlTheme,
  )
where

import Juvix.Data.HtmlTheme
import Juvix.Prelude

data HtmlOptions = HtmlOptions
  { _htmlOptionsKind :: HtmlKind,
    _htmlOptionsAssetsPrefix :: Text,
    _htmlOptionsUrlPrefix :: Text,
    _htmlOptionsOutputDir :: Path Abs Dir,
    _htmlOptionsParamBase :: Text,
    _htmlOptionsTheme :: HtmlTheme,
    _htmlOptionsNoFooter :: Bool
  }

data HtmlKind
  = HtmlDoc
  | HtmlSrc
  | HtmlOnly
  deriving stock (Data)

makeLenses ''HtmlOptions
