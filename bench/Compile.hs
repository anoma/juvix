module Compile where

import Base
import Development.Shake
import Juvix.Prelude.Base
import Juvix.Prelude.Path as Path hiding (doesFileExist, (-<.>))

dirs :: [Path Rel Dir]
dirs =
  [ $(mkRelDir "ackermann"),
    $(mkRelDir "combinations"),
    $(mkRelDir "cps"),
    $(mkRelDir "fibonacci"),
    $(mkRelDir "fold"),
    $(mkRelDir "mapfold"),
    $(mkRelDir "mapfun"),
    $(mkRelDir "maybe"),
    $(mkRelDir "mergesort"),
    $(mkRelDir "prime")
  ]

absDirs :: [Path Abs Dir]
absDirs = map (root Path.<//>) dirs

compile :: IO ()
compile = shakeArgs opts $ do
  phony "clean" $ do
    putInfo "TODO: Cleaning... "
  forM_ absDirs mkBenchMark
  where
    opts :: ShakeOptions
    opts = shakeOptions

recipe :: Path Abs File -> Action () -> Rules ()
recipe out howto = toFilePath out %> const howto

-- | e.g. dir = fibonacci
mkBenchMark :: Path Abs Dir -> Rules ()
mkBenchMark dir = do
  mkHaskell dir
  mkOcaml dir
  mkJuvix dir
  mkJuvixRuntime dir

-- mkClang dir FIXME

-- | e.g. dir = fibonacci
mkHaskell :: Path Abs Dir -> Rules ()
mkHaskell dir = do
  action $
    whenM
      (doesFileExist (toFilePath hsFile))
      (need [toFilePath exeFile, toFilePath sexeFile])
  let opts :: [String] = ["-O2", "-no-keep-hi-files", "-no-keep-o-files"]
  recipe exeFile $ do
    need [toFilePath hsFile]
    command_ [] "ghc" (opts ++ ["-o", toFilePath exeFile, toFilePath hsFile])
  recipe sexeFile $ do
    need [toFilePath hsFile]
    command_ [] "ghc" (opts ++ ["-XStrict", "-o", toFilePath sexeFile, toFilePath hsFile])
  where
    haskellDir :: Path Abs Dir
    haskellDir = dir Path.<//> $(mkRelDir "haskell")
    hsFile :: Path Abs File
    hsFile = addExtension' ".hs" (haskellDir Path.<//> dirnameToFile dir)
    exeFile :: Path Abs File
    exeFile = replaceExtension' ".exe" hsFile
    sexeFile :: Path Abs File
    sexeFile = addExtension' ".exe" (replaceExtension' ".strict" hsFile)

-- | e.g. dir = fibonacci
mkOcaml :: Path Abs Dir -> Rules ()
mkOcaml dir = do
  action $
    whenM
      (doesFileExist (toFilePath mlFile))
      (need [toFilePath exeFile, toFilePath byteexeFile])
  recipe exeFile $ do
    need [toFilePath mlFile]
    command_ [] "ocamlopt" ["-O2", "-o", toFilePath exeFile, toFilePath mlFile]
  recipe byteexeFile $ do
    need [toFilePath mlFile]
    command_ [] "ocamlc" ["-o", toFilePath byteexeFile, toFilePath mlFile]
  where
    ocamlDir :: Path Abs Dir
    ocamlDir = dir Path.<//> $(mkRelDir "ocaml")
    mlFile :: Path Abs File
    mlFile = addExtension' ".ml" (ocamlDir Path.<//> dirnameToFile dir)
    exeFile :: Path Abs File
    exeFile = replaceExtension' ".exe" mlFile
    byteexeFile :: Path Abs File
    byteexeFile = addExtension' ".exe" (replaceExtension' ".byte" mlFile)

-- | e.g. dir = fibonacci
mkJuvix :: Path Abs Dir -> Rules ()
mkJuvix dir = do
  action $
    whenM
      (doesFileExist (toFilePath juvixFile))
      (need [toFilePath exeFile, toFilePath wasmFile])
  let opts :: [String] = ["compile"]
  recipe exeFile $ do
    need [toFilePath juvixFile]
    command_ [] "juvix" (opts <> ["-o", toFilePath exeFile, toFilePath juvixFile])
  recipe wasmFile $ do
    need [toFilePath juvixFile]
    command_ [] "juvix" (opts <> ["--target=wasm", "-o", toFilePath wasmFile, toFilePath juvixFile])
  where
    ocamlDir :: Path Abs Dir
    ocamlDir = dir Path.<//> $(mkRelDir "juvix")
    juvixFile :: Path Abs File
    juvixFile = addExtension' ".juvix" (ocamlDir Path.<//> dirnameToFile dir)
    exeFile :: Path Abs File
    exeFile = replaceExtension' ".exe" juvixFile
    wasmFile :: Path Abs File
    wasmFile = replaceExtension' ".wasm" juvixFile

-- | e.g. dir = fibonacci
mkJuvixRuntime :: Path Abs Dir -> Rules ()
mkJuvixRuntime dir = do
  action $
    whenM
      (doesFileExist (toFilePath cFile))
      (need [toFilePath exeFile, toFilePath wasmFile])
  let opts :: [String] = ["dev", "runtime", "compile"]
  recipe exeFile $ do
    need [toFilePath cFile]
    command_ [] "juvix" (opts <> ["-o", toFilePath exeFile, toFilePath cFile])
  recipe wasmFile $ do
    need [toFilePath cFile]
    command_ [] "juvix" (opts <> ["--target=wasm32-wasi", "-o", toFilePath wasmFile, toFilePath cFile])
  where
    runtimeDir :: Path Abs Dir
    runtimeDir = dir Path.<//> $(mkRelDir "runtime")
    cFile :: Path Abs File
    cFile = addExtension' ".c" (runtimeDir Path.<//> dirnameToFile dir)
    exeFile :: Path Abs File
    exeFile = replaceExtension' ".exe" cFile
    wasmFile :: Path Abs File
    wasmFile = replaceExtension' ".wasm" cFile

mkClang :: Path Abs Dir -> Rules ()
mkClang dir = do
  action $
    whenM
      (doesFileExist (toFilePath cFile))
      (need [toFilePath exeFile, toFilePath wasmFile])
  recipe exeFile $ do
    need [toFilePath cFile]
    command_ [] "clang" ["-O3", "-o", toFilePath exeFile, toFilePath cFile]
  recipe wasmFile $ do
    need [toFilePath cFile]
    command_ [] "clang" ["-Os", "-nodefaultlibs", "--sysroot", "$WASI_SYSROOT_Path", "-lc", "--target=wasm32-wasi", "-o", toFilePath wasmFile, toFilePath cFile]
  where
    cDir :: Path Abs Dir
    cDir = dir Path.<//> $(mkRelDir "c")
    cFile :: Path Abs File
    cFile = addExtension' ".c" (cDir Path.<//> dirnameToFile dir)
    exeFile :: Path Abs File
    exeFile = replaceExtension' ".exe" cFile
    wasmFile :: Path Abs File
    wasmFile = replaceExtension' ".wasm" cFile
