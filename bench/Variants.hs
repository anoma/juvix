module Variants where

import Base
import Data.Colour
import Data.Colour.Palette.BrewerSet
import Development.Shake hiding ((<//>))
import Juvix.Prelude
import Juvix.Prelude.Env
import System.Process

allVariants :: [Variant]
allVariants = map getVariant allElements

defaultVariants :: [Variant]
defaultVariants = map getVariant (delete CoreEval allElements)

data VariantId
  = OcamlExe
  | OcamlByte
  | HaskellExe
  | HaskellStrict
  | JuvixExe
  | JuvixWasm
  | ClangExe
  | ClangWasm
  | RuntimeExe
  | RuntimeWasm
  | CoreEval
  deriving stock (Bounded, Enum, Eq, Ord)

getVariantIx :: VariantId -> Int
getVariantIx = fromEnum

-- | Note that only 12 colors are available
getVariantColor :: VariantId -> Colour Double
getVariantColor v
  | i < 12 = brewerSet Paired 12 !! i
  | otherwise = error "not enough colors. Please extend the palette"
  where
    i :: Int
    i = getVariantIx v

getVariant :: VariantId -> Variant
getVariant = \case
  OcamlExe -> ocamlExe
  OcamlByte -> ocamlByteExe
  HaskellExe -> haskellExe
  HaskellStrict -> haskellStrictExe
  JuvixExe -> juvixExe
  JuvixWasm -> juvixWasm
  ClangExe -> clangExe
  ClangWasm -> clangWasm
  RuntimeExe -> runtimeExe
  RuntimeWasm -> runtimeWasm
  CoreEval -> coreEval

defaultExt :: [String]
defaultExt = [".exe"]

runWasm :: Path Abs File -> IO ()
runWasm p = void (readProcess "wasmer" [toFilePath p, "--disable-cache"] "")

runExe :: Path Abs File -> IO ()
runExe p = void (readProcess (toFilePath p) [] "")

outOptions :: BuildArgs -> [String] -> [String]
outOptions args ext = ["-o", toFilePath (binFile args ext)]

commonOptions :: BuildArgs -> [String] -> [String]
commonOptions args ext = toFilePath (args ^. buildSrc) : outOptions args ext

ocamlExe :: Variant
ocamlExe =
  Variant
    { _variantTitle = Nothing,
      _variantLanguage = Ocaml,
      _variantExtensions = ext,
      _variantColor = getVariantColor OcamlExe,
      _variantRun = runExe,
      _variantBuild = \args ->
        command_ [] "ocamlopt" ("-O2" : commonOptions args ext)
    }
  where
    ext :: [String]
    ext = defaultExt

ocamlByteExe :: Variant
ocamlByteExe =
  Variant
    { _variantTitle = Just "byte",
      _variantLanguage = Ocaml,
      _variantExtensions = ext,
      _variantColor = getVariantColor OcamlByte,
      _variantRun = runExe,
      _variantBuild = \args ->
        command_ [] "ocamlc" (commonOptions args ext)
    }
  where
    ext :: [String]
    ext = ".byte" : defaultExt

haskellCommon :: [String]
haskellCommon = ["-O2", "-no-keep-hi-files", "-no-keep-o-files"]

haskellExe :: Variant
haskellExe =
  Variant
    { _variantTitle = Nothing,
      _variantLanguage = Haskell,
      _variantExtensions = ext,
      _variantColor = getVariantColor HaskellExe,
      _variantRun = runExe,
      _variantBuild = \args ->
        command_ [] "ghc" (haskellCommon ++ commonOptions args ext)
    }
  where
    ext :: [String]
    ext = defaultExt

haskellStrictExe :: Variant
haskellStrictExe =
  Variant
    { _variantTitle = Just "strict",
      _variantLanguage = Haskell,
      _variantExtensions = ext,
      _variantColor = getVariantColor HaskellStrict,
      _variantRun = runExe,
      _variantBuild = \args ->
        command_ [] "ghc" (haskellCommon ++ ["-XStrict"] ++ commonOptions args ext)
    }
  where
    ext :: [String]
    ext = ".strict" : defaultExt

juvixExe :: Variant
juvixExe =
  Variant
    { _variantTitle = Nothing,
      _variantLanguage = Juvix,
      _variantExtensions = ext,
      _variantColor = getVariantColor JuvixExe,
      _variantRun = runExe,
      _variantBuild = \args ->
        command_ [] "juvix" (juvixCommon ++ commonOptions args ext)
    }
  where
    ext :: [String]
    ext = defaultExt

juvixCommon :: [String]
juvixCommon = ["compile"]

juvixWasm :: Variant
juvixWasm =
  Variant
    { _variantTitle = Just "wasm",
      _variantLanguage = Juvix,
      _variantExtensions = ext,
      _variantColor = getVariantColor JuvixWasm,
      _variantRun = runWasm,
      _variantBuild = \args ->
        command_ [] "juvix" (juvixCommon ++ ["--target=wasm"] ++ commonOptions args ext)
    }
  where
    ext :: [String]
    ext = [".wasm"]

runtimeCommon :: [String]
runtimeCommon = ["dev", "runtime", "compile"]

runtimeExe :: Variant
runtimeExe =
  Variant
    { _variantTitle = Nothing,
      _variantLanguage = Runtime,
      _variantExtensions = ext,
      _variantColor = getVariantColor RuntimeExe,
      _variantRun = runExe,
      _variantBuild = \args ->
        command_ [] "juvix" (runtimeCommon ++ commonOptions args ext)
    }
  where
    ext :: [String]
    ext = defaultExt

runtimeWasm :: Variant
runtimeWasm =
  Variant
    { _variantTitle = Just "wasm",
      _variantLanguage = Runtime,
      _variantExtensions = ext,
      _variantColor = getVariantColor RuntimeWasm,
      _variantRun = runWasm,
      _variantBuild = \args ->
        command_ [] "juvix" (runtimeCommon ++ ["--target=wasm32-wasi"] ++ commonOptions args ext)
    }
  where
    ext :: [String]
    ext = [".wasm"]

clangExe :: Variant
clangExe =
  Variant
    { _variantTitle = Nothing,
      _variantLanguage = C,
      _variantExtensions = ext,
      _variantColor = getVariantColor ClangExe,
      _variantRun = runExe,
      _variantBuild = \args ->
        command_ [] "clang" ("-O3" : commonOptions args ext)
    }
  where
    ext :: [String]
    ext = defaultExt

clangWasm :: Variant
clangWasm =
  Variant
    { _variantTitle = Just "wasm",
      _variantLanguage = C,
      _variantExtensions = ext,
      _variantColor = getVariantColor ClangWasm,
      _variantRun = runWasm,
      _variantBuild = \args -> do
        wasipath <- getWasiSysrootPathStr
        command_ [] "clang" (["-Os", "-nodefaultlibs", "--sysroot", wasipath, "-lc", "--target=wasm32-wasi"] ++ commonOptions args ext)
    }
  where
    ext :: [String]
    ext = [".wasm"]

coreEval :: Variant
coreEval =
  Variant
    { _variantTitle = Nothing,
      _variantLanguage = Core,
      _variantExtensions = ext,
      _variantColor = getVariantColor CoreEval,
      _variantRun = \src -> void (readProcess "juvix" ["dev", "core", "eval", toFilePath src] ""),
      _variantBuild = \args ->
        command_ [] "cp" (map toFilePath [args ^. buildSrc, binFile args ext])
    }
  where
    ext :: [String]
    ext = [langExtension Core]
