module Commands.Doctor where

import Data.Aeson
import Data.Aeson.TH
import Juvix.Extra.Version qualified as V
import Juvix.Prelude
import Network.HTTP.Simple
import Options.Applicative
import Safe (headMay)
import System.Environment qualified as E
import System.Process qualified as P
import Text.Read (readMaybe)

newtype GithubRelease = GithubRelease {_githubReleaseTagName :: Maybe Text}
  deriving stock (Eq, Show, Generic)

$( deriveFromJSON
     defaultOptions
       { fieldLabelModifier = camelTo2 '_' . dropPrefix "_githubRelease"
       }
     ''GithubRelease
 )

newtype DoctorOptions = DoctorOptions
  { _doctorOffline :: Bool
  }

makeLenses ''GithubRelease
makeLenses ''DoctorOptions

parseDoctorOptions :: Parser DoctorOptions
parseDoctorOptions = do
  _doctorOffline <-
    switch
      ( long "offline"
          <> help "Run the doctor offline"
      )
  pure DoctorOptions {..}

type DoctorEff = '[Log, Embed IO]

minimumClangVersion :: Integer
minimumClangVersion = 13

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

checkClangVersion :: Members DoctorEff r => Integer -> Text -> Sem r ()
checkClangVersion expectedVersion errMsg = do
  versionString <- embed (P.readProcess "clang" ["-dumpversion"] "")
  case headMay (splitOn "." versionString) >>= readMaybe of
    Just majorVersion -> unless (majorVersion >= expectedVersion) (log errMsg)
    Nothing -> log "  ! Could not determine clang version"

checkEnvVarSet :: Members DoctorEff r => String -> Text -> Sem r ()
checkEnvVarSet var errMsg = do
  whenM (isNothing <$> embed (E.lookupEnv var)) (log errMsg)

getLatestRelease :: Members '[Embed IO, Fail] r => Sem r GithubRelease
getLatestRelease = do
  request' <- failFromException (parseRequest "https://api.github.com/repos/anoma/juvix/releases/latest")
  let request = setRequestHeaders [("user-agent", "curl/7.79.1"), ("Accept", "application/vnd.github+json")] request'
  response <- failFromException (httpJSON request)
  return (getResponseBody response)

checkVersion :: Members DoctorEff r => Sem r ()
checkVersion = do
  log "> Checking latest Juvix release on Github..."
  let tagName = "v" <> V.versionDoc
  response <- runFail getLatestRelease
  case response of
    Just release -> case release ^. githubReleaseTagName of
      Just latestTagName -> unless (tagName == latestTagName) (log ("  ! Newer Juvix version is available from https://github.com/anoma/juvix/releases/tag/" <> latestTagName))
      Nothing -> log "  ! Tag name is not present in release JSON from Github API"
    Nothing -> log "  ! Network error when fetching data from Github API"

checkClang :: Members DoctorEff r => Sem r ()
checkClang = do
  log "> Checking for clang..."
  checkCmdOnPath "clang" "  ! Could not find the clang command"
  log "> Checking clang version..."
  checkClangVersion minimumClangVersion ("  ! Clang version " <> show minimumClangVersion <> " or newer required")
  log "> Checking for wasm-ld..."
  checkCmdOnPath "wasm-ld" "  ! Could not find the wasm-ld command"
  log "> Checking that clang supports wasm32..."
  checkClangTargetSupported "wasm32" "  ! Clang does not support the wasm32 target"
  log "> Checking that clang supports wasm32-wasi..."
  checkClangTargetSupported "wasm32-wasi" "  ! Clang does not support the wasm32-wasi target"
  log "> Checking that WASI_SYSROOT_PATH is set..."
  checkEnvVarSet "WASI_SYSROOT_PATH" "  ! Environment variable WASI_SYSROOT_PATH is missing"

checkWasmer :: Members DoctorEff r => Sem r ()
checkWasmer = do
  log "> Checking for wasmer..."
  checkCmdOnPath "wasmer" "  ! Could not find the wasmer command"

doctor :: Members DoctorEff r => DoctorOptions -> Sem r ()
doctor opts = do
  checkClang
  checkWasmer
  unless (opts ^. doctorOffline) checkVersion
