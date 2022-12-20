module Compile where

import Base
import Gauge
import System.Process
import Development.Shake hiding ((<//>))
import Juvix.Prelude.Base
import Juvix.Prelude.Path as Path hiding (doesFileExist, (-<.>))
import Juvix.Prelude.Path qualified as Path
import Suites

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
  action $
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
          _buildOutDir = variantBinDir s v
        }
    srcDir :: Path Abs Dir
    srcDir = suiteSrcDir s
    srcFile :: Path Abs File
    srcFile = addExtension' (langExtension (v ^. variantLanguage)) (srcDir Path.<//> suiteBaseFile s)
    exeFile :: Path Abs File
    exeFile = replaceExtensions' (v ^. variantExtensions) srcFile

csvRules :: Suite -> Rules ()
csvRules s = do
  let csv :: Path Abs File = suiteCsvFile s
  recipe csv $ do
    need [ toFilePath (variantBinFile s v) | v <- s ^. suiteVariants  ]
    ensureDir (parent csv)
    whenM (Path.doesFileExist csv) (removeFile csv)
    liftIO (runMode DefaultMode (config s) [] [fromSuite s])

runExe :: Path Abs File -> IO ()
runExe p = void (readProcess (toFilePath p) [] "")

fromSuite :: Suite -> Benchmark
fromSuite b = bgroup (b ^. suiteTitle) (map go (b ^. suiteVariants))
  where
    go :: Variant -> Benchmark
    go v = bench title (nfIO (runExe exe))
      where
        title :: String
        title = show (v ^. variantLanguage) <> maybe "" (" " <>) (v ^. variantTitle)
        exe :: Path Abs File
        exe =
          addExtensions'
            (v ^. variantExtensions)
            ( root
                <//> relDir (b ^. suiteTitle)
                <//> langPath (v ^. variantLanguage)
                <//> relFile (b ^. suiteTitle)
            )
config :: Suite -> Config
config s =
  defaultConfig
    { timeLimit = Just 0.0,
      quickMode = False,
      csvFile = Just (toFilePath (suiteCsvFile s))
    }
