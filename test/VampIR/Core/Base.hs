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
    Right tab -> do
      withTempDir'
        ( \dirPath -> do
            step "Translate to VampIR"
            let vampirFile = dirPath <//> $(mkRelFile "program.pir")
            case run (runReader defaultCoreOptions (runError @JuvixError (coreToVampIR' tab))) of
              Left err -> assertFailure (show (pretty (fromJuvixError @GenericError err)))
              Right VampIR.Result {..} -> do
                TIO.writeFile (toFilePath vampirFile) _resultCode
                vampirAssertion' vampirFile dataFile step
        )

vampirAssertion' :: Path Abs File -> Path Abs File -> (String -> IO ()) -> Assertion
vampirAssertion' inputFile dataFile step = do
  step "Check vamp-ir on path"
  assertCmdExists $(mkRelFile "vamp-ir")

  withTempDir'
    ( \dirPath -> do
        step "VampIR setup parameters"
        P.callProcess "vamp-ir" (setupParamsArgs dirPath)
        step "VampIR compile"
        P.callProcess "vamp-ir" (compileArgs inputFile dirPath)
        step "VampIR prove"
        P.callProcess "vamp-ir" (proveArgs dataFile dirPath)
        step "VampIR verify"
        P.callProcess "vamp-ir" (verifyArgs dirPath)
    )

setupParamsArgs :: Path Abs Dir -> [String]
setupParamsArgs dirPath =
  [ "plonk",
    "setup",
    "-o",
    toFilePath (dirPath <//> $(mkRelFile "params.pp"))
  ]

compileArgs :: Path Abs File -> Path Abs Dir -> [String]
compileArgs inputFile dirPath =
  [ "plonk",
    "compile",
    "-u",
    toFilePath (dirPath <//> $(mkRelFile "params.pp")),
    "-s",
    toFilePath inputFile,
    "-o",
    toFilePath (dirPath <//> $(mkRelFile "circuit.plonk"))
  ]

proveArgs :: Path Abs File -> Path Abs Dir -> [String]
proveArgs dataFile dirPath =
  [ "plonk",
    "prove",
    "-u",
    toFilePath (dirPath <//> $(mkRelFile "params.pp")),
    "-c",
    toFilePath (dirPath <//> $(mkRelFile "circuit.plonk")),
    "-o",
    toFilePath (dirPath <//> $(mkRelFile "proof.plonk")),
    "-i",
    toFilePath dataFile
  ]

verifyArgs :: Path Abs Dir -> [String]
verifyArgs dirPath =
  [ "plonk",
    "verify",
    "-u",
    toFilePath (dirPath <//> $(mkRelFile "params.pp")),
    "-c",
    toFilePath (dirPath <//> $(mkRelFile "circuit.plonk")),
    "-p",
    toFilePath (dirPath <//> $(mkRelFile "proof.plonk"))
  ]
