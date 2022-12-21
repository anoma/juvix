module Variants where

import Base
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
      _variantColor = 0,
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
      _variantColor = 1,
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
      _variantColor = 2,
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
      _variantColor = 3,
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
      _variantColor = 4,
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
      _variantColor = 5,
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
      _variantColor = 6,
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
      _variantColor = 7,
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
      _variantColor = 8,
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
      _variantColor = 9,
      _variantBuild = \args -> do
        wasipath <- getWasiSysrootPathStr
        command_ [] "clang" (["-Os", "-nodefaultlibs", "--sysroot", wasipath, "-lc", "--target=wasm32-wasi"] ++ commonOptions args ext)
    }
  where
    ext :: [String]
    ext = defaultExt
