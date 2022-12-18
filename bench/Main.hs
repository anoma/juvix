module Main where

import Base
import Compile qualified
import Gauge.Benchmark
import Gauge.Main
import Juvix.Prelude
import Juvix.Prelude.Path as Path
import System.Process

main :: IO ()
main = do
  Compile.compile
  defaultMain benchmarks

runExe :: Path Abs File -> IO ()
runExe p = void (readProcess (toFilePath p) [] "")

hsm :: Path Abs File
hsm = root Path.<//> $(mkRelFile "mergesort/haskell/mergesort.exe")

hssm :: Path Abs File
hssm = root Path.<//> $(mkRelFile "mergesort/haskell/mergesort.strict.exe")

cm :: Path Abs File
cm = root Path.<//> $(mkRelFile "mergesort/c/mergesort.exe")

mlm :: Path Abs File
mlm = root Path.<//> $(mkRelFile "mergesort/ocaml/mergesort.exe")

jm :: Path Abs File
jm = root Path.<//> $(mkRelFile "mergesort/juvix/mergesort.exe")

benchmarks :: [Benchmark]
benchmarks =
  [ bench "haskell mergesort" (nfIO (runExe hsm)),
    bench "haskell strict mergesort" (nfIO (runExe hssm)),
    bench "c mergesort" (nfIO (runExe cm)),
    bench "juvix mergesort" (nfIO (runExe jm)),
    bench "ocaml mergesort" (nfIO (runExe mlm))
  ]
