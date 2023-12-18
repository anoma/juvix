module BackendGeb.Out.Base where

import Base
import Data.Text.IO qualified as TIO
import Juvix.Compiler.Backend.Geb.Translation qualified as Geb
import Juvix.Compiler.Core
import Juvix.Prelude.Pretty
import System.Process qualified as P
import Juvix.Compiler.Pipeline

data VampirBackend = VampirHalo2 | VampirPlonk

gebAssertion :: VampirBackend -> Path Abs File -> Path Abs File -> (String -> IO ()) -> Assertion
gebAssertion backend mainFile dataFile step = do
  step "Parse"
  s <- readFile (toFilePath mainFile)
  case runParserMain mainFile emptyInfoTable s of
    Left err -> assertFailure (show err)
    Right tab -> do
      gebAssertion' backend tab dataFile step

gebAssertion' :: VampirBackend -> InfoTable -> Path Abs File -> (String -> IO ()) -> Assertion
gebAssertion' backend tab dataFile step = do
  withTempDir'
    ( \dirPath -> do
        step "Translate to STLC"
        let gebFile = dirPath <//> $(mkRelFile "geb.lisp")
            --- TODO: choose compatible pkg and entry parameters to generate expected geb package
            packageSpec = Geb.LispPackage (Geb.LispPackageSpec "pkg" "*entry*")
        case run (runReader defaultCoreOptions (runError @JuvixError (coreToGeb' packageSpec tab))) of
          Left err -> assertFailure (show (pretty (fromJuvixError @GenericError err)))
          Right Geb.Result {..} -> do
            TIO.writeFile (toFilePath gebFile) _resultCode

            step "Check geb on path"
            assertCmdExists $(mkRelFile "geb.image")

            step "Translate to VampIR through the Geb binary"
            let vampirOutputFile = dirPath <//> $(mkRelFile "program.pir")
            P.callProcess "geb.image" (gebp gebFile vampirOutputFile)

            step "Check vamp-ir on path"
            assertCmdExists $(mkRelFile "vamp-ir")

            vampirSetupArgs backend dirPath step

            step "VampIR compile"
            P.callProcess "vamp-ir" (compileArgs vampirOutputFile dirPath backend)
            step "VampIR prove"
            P.callProcess "vamp-ir" (proveArgs dataFile dirPath backend)
            step "VampIR verify"
            P.callProcess "vamp-ir" (verifyArgs dirPath backend)
    )
    where
      gebp :: Path Abs File -> Path Abs File -> [String]
      gebp gebfile vampfile =
        [ "-i",
          toFilePath gebfile,
          "-e",
          "pkg::*entry*",
          "-l",
          "-s",
          "-p",
          "-t",
          "-o",
          toFilePath vampfile
        ]

vampirSetupArgs :: VampirBackend -> Path Abs Dir -> (String -> IO ()) -> Assertion
vampirSetupArgs VampirHalo2 _ _ = return ()
vampirSetupArgs VampirPlonk dirPath step = do
  step "VampIR setup parameters"
  P.callProcess "vamp-ir" setupParamsArgs
  where
    setupParamsArgs =
      [ "-q",
        "plonk",
        "setup",
        "-m",
        "9",
        "-o",
        toFilePath (dirPath <//> $(mkRelFile "params.pp"))
      ]

compileArgs :: Path Abs File -> Path Abs Dir -> VampirBackend -> [String]
compileArgs inputFile dirPath = \case
  VampirHalo2 ->
    [ "-q",
      "halo2",
      "compile",
      "-s",
      toFilePath inputFile,
      "-o",
      toFilePath (dirPath <//> $(mkRelFile "circuit.halo2"))
    ]
  VampirPlonk ->
    [ "-q",
      "plonk",
      "compile",
      "-u",
      toFilePath (dirPath <//> $(mkRelFile "params.pp")),
      "-s",
      toFilePath inputFile,
      "-o",
      toFilePath (dirPath <//> $(mkRelFile "circuit.plonk"))
    ]

proveArgs :: Path Abs File -> Path Abs Dir -> VampirBackend -> [String]
proveArgs dataFile dirPath = \case
  VampirHalo2 ->
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
  VampirPlonk ->
    [ "-q",
      "plonk",
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

verifyArgs :: Path Abs Dir -> VampirBackend -> [String]
verifyArgs dirPath = \case
  VampirHalo2 ->
    [ "-q",
      "halo2",
      "verify",
      "-c",
      toFilePath (dirPath <//> $(mkRelFile "circuit.halo2")),
      "-p",
      toFilePath (dirPath <//> $(mkRelFile "proof.halo2"))
    ]
  VampirPlonk ->
    [ "-q",
      "plonk",
      "verify",
      "-u",
      toFilePath (dirPath <//> $(mkRelFile "params.pp")),
      "-c",
      toFilePath (dirPath <//> $(mkRelFile "circuit.plonk")),
      "-p",
      toFilePath (dirPath <//> $(mkRelFile "proof.plonk"))
    ]
