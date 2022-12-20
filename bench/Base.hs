module Base where

import Juvix.Extra.Paths
import Juvix.Prelude
import Prelude (Show (show))
import Development.Shake hiding ((<//>))

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/benchmark")

resultsDir :: Path Abs Dir
resultsDir = relToProject $(mkRelDir ".benchmark-results")

resultDirs :: [Path Abs Dir]
resultDirs = [binDir, plotDir, csvDir]

binDir :: Path Abs Dir
binDir = resultsDir <//> $(mkRelDir "bin")

plotDir :: Path Abs Dir
plotDir = resultsDir <//> $(mkRelDir "plot")

csvDir :: Path Abs Dir
csvDir = resultsDir <//> $(mkRelDir "csv")

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

langExtension :: Lang -> String
langExtension = \case
    Ocaml -> ".ml"
    Haskell -> ".hs"
    C -> ".c"
    Juvix -> ".juvix"
    Runtime -> ".c"
    Core -> ".jvc"

data Variant = Variant
  { _variantTitle :: Maybe String,
    _variantLanguage :: Lang,
    _variantExtensions :: [String],
    _variantBuild :: BuildArgs -> Action ()
  }

data BuildArgs = BuildArgs {
  _buildSrc :: Path Abs File,
  _buildOutDir :: Path Abs Dir
  }

data Suite = Suite
  { _suiteTitle :: String,
    _suiteVariants :: [Variant]
  }

makeLenses ''Suite
makeLenses ''BuildArgs
makeLenses ''Variant

suiteCsvFile :: Suite -> Path Abs File
suiteCsvFile s = addExtension' ".csv" (csvDir <//> suiteBaseFile s)

suiteSrcDir :: Suite -> Path Abs Dir
suiteSrcDir s = root <//> relDir (s ^. suiteTitle)

suiteBaseFile :: Suite -> Path Rel File
suiteBaseFile s = relFile (s ^. suiteTitle)

variantSrcDir :: Suite -> Variant -> Path Abs Dir
variantSrcDir s v = suiteSrcDir s <//> langPath (v ^. variantLanguage)

suitePath :: Suite -> Path Rel Dir
suitePath s = relDir (s ^. suiteTitle)

variantBinDir :: Suite -> Variant -> Path Abs Dir
variantBinDir s v = binDir <//> suitePath s <//> langPath (v ^. variantLanguage)

variantBinFile :: Suite -> Variant -> Path Abs File
variantBinFile s v = variantBinDir s v <//> addExtensions' (v ^. variantExtensions) (suiteBaseFile s)
