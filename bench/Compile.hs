module Compile where

import Base
import Development.Shake hiding ((<//>))
import Gauge
import Juvix.Prelude.Base
import Juvix.Prelude.Path as Path hiding (doesFileExist, (-<.>))
import Juvix.Prelude.Path qualified as Path
import Suites
import System.Process

compile :: IO ()
compile = shakeArgs opts compileRules
  where
    opts :: ShakeOptions
    opts = shakeOptions

compileRules :: Rules ()
compileRules = do
  phony "clean" $ do
    putInfo "TODO: Cleaning... "
  forM_ suites suiteRules

suiteRules :: Suite -> Rules ()
suiteRules s = do
  forM_ (s ^. suiteVariants) (variantRules s)
  csvRules s

recipe :: Path Abs File -> Action () -> Rules ()
recipe out howto = toFilePath out %> const howto

variantRules :: Suite -> Variant -> Rules ()
variantRules s v = do
  action $ do
    whenM
      (doesFileExist (toFilePath srcFile))
      (need [toFilePath exeFile])

  recipe exeFile $ do
    need [toFilePath srcFile]
    ensureDir outDir
    (v ^. variantBuild) args
  where
    args :: BuildArgs
    args =
      BuildArgs
        { _buildSrc = srcFile,
          _buildOutDir = outDir
        }
    lang :: Lang
    lang = v ^. variantLanguage
    srcFile :: Path Abs File
    srcFile =
      addExtension'
        (langExtension lang)
        (suiteSrcDir s <//> langPath lang <//> suiteBaseFile s)
    exeFile :: Path Abs File
    exeFile = outDir <//> replaceExtensions' (v ^. variantExtensions) (filename srcFile)
    outDir :: Path Abs Dir
    outDir = variantBinDir s v

csvRules :: Suite -> Rules ()
csvRules s = do
  let csv :: Path Abs File = suiteCsvFile s
  want [toFilePath csv]
  recipe csv $ do
    need [toFilePath (variantBinFile s v) | v <- s ^. suiteVariants]
    ensureDir (parent csv)
    whenM (Path.doesFileExist csv) (removeFile csv)
    liftIO (runMode DefaultMode (config s) [] [fromSuite s])

runExe :: Path Abs File -> IO ()
runExe p = void (readProcess (toFilePath p) [] "")

fromSuite :: Suite -> Benchmark
fromSuite s = bgroup (s ^. suiteTitle) (map go (s ^. suiteVariants))
  where
    go :: Variant -> Benchmark
    go v = bench title (nfIO (runExe (variantBinFile s v)))
      where
        title :: String
        title = show (v ^. variantLanguage) <> maybe "" (" " <>) (v ^. variantTitle)

config :: Suite -> Config
config s =
  defaultConfig
    { timeLimit = Just 0.0,
      quickMode = False,
      csvFile = Just (toFilePath (suiteCsvFile s))
    }
