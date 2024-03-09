module Juvix.Compiler.Backend.Html.Data.Options where

import Juvix.Prelude
import Prelude (show)

data HtmlOptions = HtmlOptions
  { _htmlOptionsKind :: HtmlKind,
    _htmlOptionsAssetsPrefix :: Text,
    _htmlOptionsUrlPrefix :: Text,
    _htmlOptionsIdPrefix :: Text,
    _htmlOptionsOnlyCode :: Bool,
    _htmlOptionsNoPath :: Bool,
    _htmlOptionsExt :: Text,
    _htmlOptionsStripPrefix :: Text,
    _htmlOptionsOutputDir :: Path Abs Dir,
    _htmlOptionsParamBase :: Text,
    _htmlOptionsTheme :: Theme,
    _htmlOptionsNoFooter :: Bool,
    _htmlOptionsFolderStructure :: Bool
  }

defaultHtmlOptions :: HtmlOptions
defaultHtmlOptions =
  HtmlOptions
    { _htmlOptionsKind = HtmlDoc,
      _htmlOptionsAssetsPrefix = "",
      _htmlOptionsUrlPrefix = "",
      _htmlOptionsIdPrefix = "",
      _htmlOptionsOnlyCode = False,
      _htmlOptionsExt = ".html",
      _htmlOptionsStripPrefix = "",
      _htmlOptionsNoPath = False,
      _htmlOptionsOutputDir = $(mkAbsDir "/tmp"),
      _htmlOptionsParamBase = "",
      _htmlOptionsTheme = Nord,
      _htmlOptionsNoFooter = False,
      _htmlOptionsFolderStructure = False
    }

data Theme
  = Nord
  | Ayu
  | Macchiato
  deriving stock (Enum, Eq, Ord, Bounded, Data)

instance Show Theme where
  show = \case
    Nord -> "nord"
    Ayu -> "ayu"
    Macchiato -> "macchiato"

data ThemeLight
  = Dark
  | Light
  deriving stock (Show, Eq, Enum, Ord, Bounded, Data)

themeLight :: Theme -> ThemeLight
themeLight = \case
  Nord -> Dark
  Macchiato -> Dark
  Ayu -> Light

data HtmlKind
  = HtmlDoc
  | HtmlSrc
  | HtmlOnly
  deriving stock (Data)

makeLenses ''HtmlOptions
