module Commands.Doctor where

import Juvix.Prelude
import System.Process qualified as P

type DoctorEff = '[Log, Embed IO]

checkCmdOnPath :: Members DoctorEff r => String -> Text -> Sem r ()
checkCmdOnPath cmd errMsg =
  whenM (isNothing <$> embed (findExecutable cmd)) (log errMsg)

checkClangTargetSupported :: Members DoctorEff r => String -> Text -> Sem r ()
checkClangTargetSupported target errMsg = do
  (code, _, _) <-
    embed
      ( P.readProcessWithExitCode
          "clang"
          ["-target", target, "--print-supported-cpus"]
          ""
      )
  unless (code == ExitSuccess) (log errMsg)

checkClang :: Members DoctorEff r => Sem r ()
checkClang = do
  log "> Checking for clang..."
  checkCmdOnPath "clang" "  ! Could not find the clang command"
  log "> Checking that clang supports wasm32..."
  checkClangTargetSupported "wasm32" "  ! Clang does not support the wasm32 target"
  log "> Checking that clang supports wasm32-wasi..."
  checkClangTargetSupported "wasm32-wasi" "  ! Clang does not support the wasm32-wasi target"

doctor :: Members DoctorEff r => Sem r ()
doctor = do
  log "Juvix doctor"
  checkClang
