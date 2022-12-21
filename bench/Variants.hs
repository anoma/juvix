module Variants where

import Base
import Data.Colour.Names
import Development.Shake hiding ((<//>))
import Juvix.Prelude
import Juvix.Prelude.Env

defaultVariants :: [Variant]
defaultVariants =
  [ ocamlExe,
    ocamlByteExe,
    juvixExe,
    -- juvixWasm,
    clangExe,
    -- clangWasm,
    runtimeExe,
    -- runtimeWasm,
    haskellExe,
    -- coreJvc,
    haskellStrictExe
  ]

defaultExt :: [String]
defaultExt = [".exe"]

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
      _variantColor = darkorange,
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
      _variantColor = orangered,
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
      _variantColor = purple,
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
      _variantColor = powderblue,
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
      _variantColor = plum,
      _variantBuild = \args ->
        command_ [] "juvix" (juvixCommon ++ commonOptions args ext)
    }
  where
    ext :: [String]
    ext = defaultExt

juvixCommon :: [String]
juvixCommon = ["compile"]

-- TODO does not generate an executable
juvixWasm :: Variant
juvixWasm =
  Variant
    { _variantTitle = Just "wasm",
      _variantLanguage = Juvix,
      _variantExtensions = ext,
      _variantColor = sandybrown,
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
      _variantColor = navy,
      _variantBuild = \args ->
        command_ [] "juvix" (runtimeCommon ++ commonOptions args ext)
    }
  where
    ext :: [String]
    ext = defaultExt

runtimeWasm :: Variant
runtimeWasm =
  Variant
    { _variantTitle = Nothing,
      _variantLanguage = Runtime,
      _variantExtensions = ext,
      _variantColor = turquoise,
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
      _variantColor = black,
      _variantBuild = \args ->
        command_ [] "clang" ("-O3" : commonOptions args ext)
    }
  where
    ext :: [String]
    ext = defaultExt

clangWasm :: Variant
clangWasm =
  Variant
    { _variantTitle = Nothing,
      _variantLanguage = C,
      _variantExtensions = ext,
      _variantColor = palevioletred,
      _variantBuild = \args -> do
        wasipath <- getWasiSysrootPathStr
        command_ [] "clang" (["-Os", "-nodefaultlibs", "--sysroot", wasipath, "-lc", "--target=wasm32-wasi"] ++ commonOptions args ext)
    }
  where
    ext :: [String]
    ext = defaultExt

coreJvc :: Variant
coreJvc =
  Variant
    { _variantTitle = Nothing,
      _variantLanguage = Core,
      _variantExtensions = ext,
      _variantColor = orchid,
      _variantBuild = \args ->
        command_ [] "cp" (map toFilePath [args ^. buildSrc, binFile args ext])
    }
  where
    ext :: [String]
    ext = [langExtension Core]
