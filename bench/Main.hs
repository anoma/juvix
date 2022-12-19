module Main where

import Base
import Compile qualified
import Gauge
import Juvix.Prelude
import System.Process

main :: IO ()
main = do
  Compile.compile
  defaultMainWith config benchmarks

runExe :: Path Abs File -> IO ()
runExe p = void (readProcess (toFilePath p) [] "")

config :: Config
config =
  defaultConfig
    { timeLimit = Just 0.0,
      quickMode = False,
      csvFile = Just "results.csv"
    }

defaultBench :: String -> Bench
defaultBench title =
  Bench
    { _benchTitle = title,
      _benchVariants =
        [ defaultVariant Haskell,
          defaultVariant C,
          defaultVariant Ocaml,
          defaultVariant Runtime,
          defaultVariant Juvix,
          Variant
            { _variantTitle = Just "strict",
              _variantLanguage = Haskell,
              _variantExtensions = [".strict", ".exe"]
            },
          Variant
            { _variantTitle = Just "byte",
              _variantLanguage = Ocaml,
              _variantExtensions = [".byte", ".exe"]
            }
        ]
    }

mergesort :: Bench
mergesort = defaultBench "mergesort"

fromBench :: Bench -> [Benchmark]
fromBench b = map go (b ^. benchVariants)
  where
    go :: Variant -> Benchmark
    go v = bench title (nfIO (runExe exe))
      where
        title :: String
        title = b ^. benchTitle <> maybe "" (" " <>) (v ^. variantTitle) <> " - " <> show (v ^. variantLanguage)
        exe :: Path Abs File
        exe =
          addExtensions'
            (v ^. variantExtensions)
            ( root
                <//> relDir (b ^. benchTitle)
                <//> langPath (v ^. variantLanguage)
                <//> relFile (b ^. benchTitle)
            )

benchmarks :: [Benchmark]
benchmarks = fromBench mergesort
