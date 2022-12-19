module Base where

import Juvix.Extra.Paths
import Juvix.Prelude.Path as Path
import Juvix.Prelude
import Prelude (Show (show))

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/benchmark")

data Lang
  = Ocaml
  | Haskell
  | C
  | Juvix
  | Runtime
  | Core

instance Show Lang where
  show = \case
    Ocaml -> "ocaml"
    Haskell -> "haskell"
    C -> "c"
    Juvix -> "juvix"
    Runtime -> "runtime"
    Core -> "core"

langPath :: Lang -> Path Rel Dir
langPath = relDir . Prelude.show

langFile :: Lang -> Path Rel File
langFile = relFile . Prelude.show

data Variant = Variant
  { _variantTitle :: Maybe String,
    _variantLanguage :: Lang,
    _variantExtensions :: [String]
  }

data Bench = Bench
  { _benchTitle :: String,
    _benchVariants :: [Variant]
  }

makeLenses ''Bench
makeLenses ''Variant

defaultVariant :: Lang -> Variant
defaultVariant l =
  Variant
    { _variantTitle = Nothing,
      _variantLanguage = l,
      _variantExtensions = [".exe"]
    }
