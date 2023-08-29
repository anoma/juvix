module Commands.Doctor where

import Commands.Doctor.Options
import Commands.Extra.Compile
import Data.Aeson
import Data.Aeson.TH
import Juvix.Extra.Version qualified as V
import Juvix.Prelude
import Network.HTTP.Simple
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

data DocumentedWarning
  = NoClang
  | OldClang
  | NoWasmLd
  | NoWasm32Target
  | NoWasm32WasiTarget
  | NoSysroot
  | NoWasmer

data DocumentedMessage = DocumentedMessage
  { _documentedMessageUrl :: Text,
    _documentedMessageMessage :: Text
  }

makeLenses ''GithubRelease
makeLenses ''DoctorOptions
makeLenses ''DocumentedMessage

minimumClangVersion :: Integer
minimumClangVersion = 13

documentedMessage :: DocumentedWarning -> DocumentedMessage
documentedMessage w = uncurry DocumentedMessage (first (baseUrl <>) warningInfo)
  where
    warningInfo :: (Text, Text)
    warningInfo = case w of
      NoClang -> ("could-not-find-the-clang-command", "Could not find the clang command")
      OldClang -> ("newer-clang-version-required", "Clang version " <> show minimumClangVersion <> " or newer required")
      NoWasmLd -> ("could-not-find-the-wasm-ld-command", "Could not find the wasm-ld command")
      NoWasm32Target -> ("clang-does-not-support-the-wasm32-target", "Clang does not support the wasm32 target")
      NoWasm32WasiTarget -> ("clang-does-not-support-the-wasm32-wasi-target", "Clang does not support the wasm32-wasi target")
      NoSysroot -> ("environment-variable-wasi_sysroot_path-is-not-set", "Environment variable WASI_SYSROOT_PATH is missing")
      NoWasmer -> ("could-not-find-the-wasmer-command", "Could not find the wasmer command")

    baseUrl :: Text
    baseUrl = "https://docs.juvix.org/dev/reference/tooling/doctor/#"

heading :: (Member Log r) => Text -> Sem r ()
heading = log . ("> " <>)

warning :: (Member Log r) => Text -> Sem r ()
warning = log . ("  ! " <>)

info :: (Member Log r) => Text -> Sem r ()
info = log . ("  | " <>)

type DoctorEff = '[Log, Embed IO]

checkCmdOnPath :: (Members DoctorEff r) => String -> [Text] -> Sem r ()
checkCmdOnPath cmd errMsg =
  whenM (isNothing <$> findExecutable (relFile cmd)) (mapM_ warning errMsg)

-- | Check that wasm-ld exists in the same LLVM distribution as the clang command
checkWasmLd :: (Members DoctorEff r) => Path Abs File -> [Text] -> Sem r ()
checkWasmLd clangPath errMsg =
  unlessM (isExecutable (parent clangPath <//> $(mkRelFile "wasm-ld"))) (mapM_ warning errMsg)

checkClangTargetSupported :: (Members DoctorEff r) => Path Abs File -> String -> [Text] -> Sem r ()
checkClangTargetSupported clangPath target errMsg = do
  (code, _, _) <-
    embed
      ( P.readProcessWithExitCode
          (toFilePath clangPath)
          ["-target", target, "--print-supported-cpus"]
          ""
      )
  unless (code == ExitSuccess) (mapM_ warning errMsg)

checkClangVersion :: (Members DoctorEff r) => Path Abs File -> Integer -> [Text] -> Sem r ()
checkClangVersion clangPath expectedVersion errMsg = do
  versionString <- embed (P.readProcess (toFilePath clangPath) ["-dumpversion"] "")
  case headMay (splitOn "." versionString) >>= readMaybe of
    Just majorVersion -> unless (majorVersion >= expectedVersion) (mapM_ warning errMsg)
    Nothing -> warning "Could not determine clang version"

checkEnvVarSet :: (Members DoctorEff r) => String -> [Text] -> Sem r ()
checkEnvVarSet var errMsg = do
  whenM (isNothing <$> embed (E.lookupEnv var)) (mapM_ warning errMsg)

getLatestRelease :: (Members '[Embed IO, Fail] r) => Sem r GithubRelease
getLatestRelease = do
  request' <- failFromException (parseRequest "https://api.github.com/repos/anoma/juvix/releases/latest")
  let request = setRequestHeaders [("user-agent", "curl/7.79.1"), ("Accept", "application/vnd.github+json")] request'
  response <- failFromException (httpJSON request)
  return (getResponseBody response)

checkVersion :: (Members DoctorEff r) => Sem r ()
checkVersion = do
  heading "Checking latest Juvix release on Github..."
  let tagName = "v" <> V.versionDoc
  response <- runFail getLatestRelease
  case response of
    Just release -> case release ^. githubReleaseTagName of
      Just latestTagName -> unless (tagName == latestTagName) (warning ("Newer Juvix version is available from https://github.com/anoma/juvix/releases/tag/" <> latestTagName))
      Nothing -> warning "Tag name is not present in release JSON from Github API"
    Nothing -> warning "Network error when fetching data from Github API"

renderDocumentedWarning :: DocumentedWarning -> [Text]
renderDocumentedWarning w = [dmsg ^. documentedMessageMessage, dmsg ^. documentedMessageUrl]
  where
    dmsg :: DocumentedMessage
    dmsg = documentedMessage w

documentedCheck ::
  ([Text] -> Sem r ()) -> DocumentedWarning -> Sem r ()
documentedCheck check w = check (renderDocumentedWarning w)

findClangPath :: (Members DoctorEff r) => Sem r (Maybe ClangPath)
findClangPath = findClang

checkClang :: forall r. (Members DoctorEff r) => Bool -> Sem r ()
checkClang printVerbose = do
  heading "Checking for clang..."
  clangPath <- findClangPath
  case clangPath of
    Just cp -> when printVerbose (printVerbosePath cp)
    Nothing -> mapM_ warning (renderDocumentedWarning NoClang)

  mapM_ checkClangProperties (extractClangPath <$> clangPath)

  heading "Checking that WASI_SYSROOT_PATH is set..."
  documentedCheck (checkEnvVarSet "WASI_SYSROOT_PATH") NoSysroot
  where
    checkClangProperties :: Path Abs File -> Sem r ()
    checkClangProperties p = do
      heading "Checking clang version..."
      documentedCheck (checkClangVersion p minimumClangVersion) OldClang
      heading "Checking for wasm-ld..."
      documentedCheck (checkWasmLd p) NoWasmLd
      heading "Checking that clang supports wasm32..."
      documentedCheck (checkClangTargetSupported p "wasm32") NoWasm32Target
      heading "Checking that clang supports wasm32-wasi..."
      documentedCheck (checkClangTargetSupported p "wasm32-wasi") NoWasm32WasiTarget

    baseClangMsg :: Path Abs File -> Text
    baseClangMsg p = "Found clang at " <> show p

    printVerbosePath :: ClangPath -> Sem r ()
    printVerbosePath = \case
      ClangEnvVarPath p -> info (baseClangMsg p <> (" using " <> pack llvmDistEnvironmentVar <> " environment variable"))
      ClangSystemPath p -> info (baseClangMsg p <> (" using system PATH"))

checkWasmer :: (Members DoctorEff r) => Sem r ()
checkWasmer = do
  heading "Checking for wasmer..."
  documentedCheck (checkCmdOnPath "wasmer") NoWasmer

runCommand :: (Members DoctorEff r) => DoctorOptions -> Sem r ()
runCommand opts = do
  checkClang (opts ^. doctorVerbose)
  checkWasmer
  unless (opts ^. doctorOffline) checkVersion
