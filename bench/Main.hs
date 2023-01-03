module Main where

import Base
import Criterion.Main
import Criterion.Main.Options hiding (config)
import Criterion.Types
import Data.Text qualified as Text
import Development.Shake hiding ((<//>))
import Juvix.Prelude.Base
import Juvix.Prelude.Path as Path hiding (doesFileExist, (-<.>))
import Juvix.Prelude.Path qualified as Path
import Suites

main :: IO ()
main = shakeArgs opts compileRules
  where
    opts :: ShakeOptions
    opts = shakeOptions

compileRules :: Rules ()
compileRules = do
  phony "clean" $ do
    putInfo ("Deleting " <> toFilePath resultsDir)
    removePathForcibly resultsDir
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
  let csv :: Path Abs File = suiteCsvFile s
      svg :: Path Abs File = suiteSvgFile s
      out :: Path Abs File = suitePlotFile s
  want [toFilePath svg]
  multiRecipe [svg] $ do
    need [toFilePath csv, toFilePath gnuplotFile]
    ensureDir (parent svg)
    command_
      []
      "gnuplot"
      ( gpArg "name" (s ^. suiteTitle)
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
    liftIO (runMode (Run (config s) Glob []) (fromSuite s) >> addColorColumn)
  where
    csv :: Path Abs File = suiteCsvFile s
    addColorColumn :: IO ()
    addColorColumn = do
      header :| rows <- nonEmpty' . Text.lines <$> readFile (toFilePath csv)
      let rows' =
            [ showColour (v ^. variantColor) <> "," <> r
              | (v, r) <- zipExact (s ^. suiteVariants) rows
            ]
          header' = "Color," <> header
      writeFile (toFilePath csv) (Text.unlines (header' : rows'))

fromSuite :: Suite -> [Benchmark]
fromSuite s = map go (s ^. suiteVariants)
  where
    go :: Variant -> Benchmark
    go v = bench title (nfIO ((v ^. variantRun) (variantBinFile s v)))
      where
        title :: String
        title = show (v ^. variantLanguage) <> maybe "" (" " <>) (v ^. variantTitle)

config :: Suite -> Config
config s =
  defaultConfig
    { csvFile = Just (toFilePath (suiteCsvFile s)),
      timeLimit = 30
    }
