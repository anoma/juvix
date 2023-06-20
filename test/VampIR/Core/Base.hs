module VampIR.Core.Base where

import Base
import Data.Text.IO qualified as TIO
import Juvix.Compiler.Backend.VampIR.Translation qualified as VampIR
import Juvix.Compiler.Core
import Juvix.Prelude.Pretty
import System.Process qualified as P

vampirAssertion :: Path Abs File -> Path Abs File -> (String -> IO ()) -> Assertion
vampirAssertion mainFile dataFile step = do
  step "Parse"
  s <- readFile (toFilePath mainFile)
  case runParserMain mainFile emptyInfoTable s of
    Left err -> assertFailure (show err)
    Right tab -> vampirAssertion' tab dataFile step

vampirAssertion' :: InfoTable -> Path Abs File -> (String -> IO ()) -> Assertion
vampirAssertion' tab dataFile step = do
  withTempDir'
    ( \dirPath -> do
        step "Translate to VampIR"
        let vampirFile = dirPath <//> $(mkRelFile "program.pir")
        case run (runReader defaultCoreOptions (runError @JuvixError (coreToVampIR' tab))) of
          Left err -> assertFailure (show (pretty (fromJuvixError @GenericError err)))
          Right VampIR.Result {..} -> do
            TIO.writeFile (toFilePath vampirFile) _resultCode

            step "Check vamp-ir on path"
            assertCmdExists $(mkRelFile "vamp-ir")

            step "VampIR compile"
            P.callProcess "vamp-ir" (compileArgs vampirFile dirPath)
            step "VampIR prove"
            P.callProcess "vamp-ir" (proveArgs dataFile dirPath)
            step "VampIR verify"
            P.callProcess "vamp-ir" (verifyArgs dirPath)
    )

compileArgs :: Path Abs File -> Path Abs Dir -> [String]
compileArgs inputFile dirPath =
  [ "-q",
    "halo2",
    "compile",
    "-s",
    toFilePath inputFile,
    "-o",
    toFilePath (dirPath <//> $(mkRelFile "circuit.halo2"))
  ]

proveArgs :: Path Abs File -> Path Abs Dir -> [String]
proveArgs dataFile dirPath =
  [ "-q",
    "halo2",
    "prove",
    "-c",
    toFilePath (dirPath <//> $(mkRelFile "circuit.halo2")),
    "-o",
    toFilePath (dirPath <//> $(mkRelFile "proof.halo2")),
    "-i",
    toFilePath dataFile
  ]

verifyArgs :: Path Abs Dir -> [String]
verifyArgs dirPath =
  [ "-q",
    "halo2",
    "verify",
    "-c",
    toFilePath (dirPath <//> $(mkRelFile "circuit.halo2")),
    "-p",
    toFilePath (dirPath <//> $(mkRelFile "proof.halo2"))
  ]
