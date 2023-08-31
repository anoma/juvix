module Juvix.Compiler.Backend.Html.Data.Options where

import Juvix.Prelude
import Prelude (show)

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
  deriving stock (Enum, Eq, Ord, Bounded, Data)

instance Show Theme where
  show = \case
    Nord -> "nord"
    Ayu -> "ayu"

data ThemeLight
  = Dark
  | Light
  deriving stock (Show, Eq, Enum, Ord, Bounded, Data)

themeLight :: Theme -> ThemeLight
themeLight = \case
  Nord -> Dark
  Ayu -> Light

data HtmlKind
  = HtmlDoc
  | HtmlSrc
  | HtmlOnly
  deriving stock (Data)

makeLenses ''HtmlOptions
