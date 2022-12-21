module Compile where

import Base
import Development.Shake hiding ((<//>))
import Data.Text qualified as Text
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
  plotRules s

multiRecipe :: [Path Abs File] -> Action () -> Rules ()
multiRecipe out howto = map toFilePath out &%> const howto

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

plotRules :: Suite -> Rules ()
plotRules s = do
  let pdf :: Path Abs File = suitePdfFile s
      csv :: Path Abs File = suiteCsvFile s
      svg :: Path Abs File = suiteSvgFile s
      out :: Path Abs File = suitePlotFile s
  want [toFilePath pdf, toFilePath svg]
  multiRecipe [pdf, svg] $ do
    need [toFilePath csv, toFilePath gnuplotFile]
    ensureDir (parent pdf)
    command_ [] "gnuplot" (
      gpArg "name" (s ^. suiteTitle)
      ++ gpArg "outfile" (toFilePath out)
      ++ gpArg "csvfile" (toFilePath csv)
      ++ [toFilePath gnuplotFile]
       )
  where
  gpArg :: String -> String -> [String]
  gpArg arg val = ["-e", arg <> "='" <> val <> "'"]


csvRules :: Suite -> Rules ()
csvRules s =
  recipe csv $ do
    need [toFilePath (variantBinFile s v) | v <- s ^. suiteVariants]
    ensureDir (parent csv)
    whenM (Path.doesFileExist csv) (removeFile csv)
    liftIO (runMode DefaultMode (config s) [] (fromSuite s))
    liftIO addColorColumn
  where
  csv :: Path Abs File = suiteCsvFile s
  addColorColumn :: IO ()
  addColorColumn = do
    header :| rows <- nonEmpty' . Text.lines <$> readFile (toFilePath csv)
    let rows' = [ show (v ^. variantColor) <> "," <> r | (v, r) <- zipExact (s ^. suiteVariants) rows ]
        header' = "Color," <> header
    writeFile (toFilePath csv) (Text.unlines (header' : rows'))

runExe :: Path Abs File -> IO ()
runExe p = void (readProcess (toFilePath p) [] "")

fromSuite :: Suite -> [Benchmark]
fromSuite s = map go (s ^. suiteVariants)
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
