module GlobalOptions
  ( module GlobalOptions,
    module Juvix.Data.Effect.TaggedLock,
  )
where

import CommonOptions
import Juvix.Compiler.Core.Options qualified as Core
import Juvix.Compiler.Internal.Pretty.Options qualified as Internal
import Juvix.Compiler.Pipeline
import Juvix.Compiler.Pipeline.EntryPoint.IO
import Juvix.Data.Effect.TaggedLock
import Juvix.Data.Error.GenericError qualified as E
import Juvix.Data.Field

data GlobalOptions = GlobalOptions
  { _globalVerify :: Bool,
    _globalNoColors :: Bool,
    _globalVSCode :: Bool,
    _globalShowNameIds :: Bool,
    _globalBuildDir :: Maybe (AppPath Dir),
    _globalIdeEndErrorChar :: Maybe Char,
    _globalStdin :: Bool,
    _globalNoTermination :: Bool,
    _globalNoPositivity :: Bool,
    _globalNoCoverage :: Bool,
    _globalNoStdlib :: Bool,
    _globalNoCheck :: Bool,
    _globalNumThreads :: NumThreads,
    _globalFieldSize :: Maybe Natural,
    _globalOffline :: Bool,
    _globalLogLevel :: LogLevel,
    _globalUnsafeIgnorePackageNameConflicts :: Bool,
    _globalDevShowThreadIds :: Bool
  }
  deriving stock (Eq, Show)

makeLenses ''GlobalOptions

instance CanonicalProjection GlobalOptions Internal.Options where
  project g =
    Internal.defaultOptions
      { Internal._optShowNameIds = g ^. globalShowNameIds
      }

instance CanonicalProjection GlobalOptions E.GenericOptions where
  project GlobalOptions {..} =
    E.GenericOptions
      { E._showNameIds = _globalShowNameIds
      }

instance CanonicalProjection GlobalOptions Core.CoreOptions where
  project GlobalOptions {..} =
    Core.CoreOptions
      { Core._optCheckCoverage = not _globalNoCoverage,
        Core._optUnrollLimit = defaultUnrollLimit,
        Core._optFieldSize = fromMaybe defaultFieldSize _globalFieldSize,
        Core._optOptimizationLevel = defaultOptimizationLevel,
        Core._optInliningDepth = defaultInliningDepth,
        Core._optVerify = _globalVerify
      }

defaultGlobalOptions :: GlobalOptions
defaultGlobalOptions =
  GlobalOptions
    { _globalVerify = False,
      _globalNoColors = False,
      _globalVSCode = False,
      _globalNumThreads = defaultNumThreads,
      _globalShowNameIds = False,
      _globalIdeEndErrorChar = Nothing,
      _globalNoTermination = False,
      _globalBuildDir = Nothing,
      _globalStdin = False,
      _globalNoPositivity = False,
      _globalLogLevel = LogLevelProgress,
      _globalNoCoverage = False,
      _globalNoStdlib = False,
      _globalNoCheck = False,
      _globalFieldSize = Nothing,
      _globalDevShowThreadIds = False,
      _globalUnsafeIgnorePackageNameConflicts = False,
      _globalOffline = False
    }

-- | Get a parser for global flags which can be hidden or not depending on
-- the input boolean
parseGlobalFlags :: Parser GlobalOptions
parseGlobalFlags = do
  _globalVerify <-
    switch
      ( long "verify"
          <> help "Generate Lean verification statements"
      )
  _globalNoColors <-
    switch
      ( long "no-colors"
          <> help "Disable ANSI formatting"
      )
  _globalVSCode <-
    switch
      ( long "vscode"
          <> help "Enable VSCode compatible output"
      )
  _globalBuildDir <-
    optional
      ( parseBuildDir
          ( long "internal-build-dir"
              <> help "Directory for compiler internal output"
          )
      )
  _globalStdin <-
    switch
      ( long "stdin"
          <> help "Read from Stdin"
      )
  _globalIdeEndErrorChar <-
    optional $
      option
        readMChar
        ( long "ide-end-error-char"
            <> help "End error message with the given character in order to facilitate parsing"
        )
  _globalNoTermination <-
    switch
      ( long "no-termination"
          <> help "Disable termination checking"
      )
  _globalNoPositivity <-
    switch
      ( long "no-positivity"
          <> help "Disable positivity checking for inductive types"
      )
  _globalNoCoverage <-
    switch
      ( long "no-coverage"
          <> help "Disable coverage checking for patterns"
      )
  _globalNoStdlib <-
    switch
      ( long "no-stdlib"
          <> help "Do not use the standard library"
      )
  _globalFieldSize <-
    option
      fieldSizeOpt
      ( long "field-size"
          <> value Nothing
          <> help "Field type size [cairo,small,11] (default: small)"
      )
  _globalOffline <-
    switch
      ( long "offline"
          <> help "Disable access to network resources"
      )
  _globalLogLevel <-
    option
      (enumReader Proxy)
      ( long "log-level"
          <> metavar "LOG_LEVEL"
          <> completer (enumCompleter @LogLevel Proxy)
          <> value defaultLogLevel
          <> help
            ( "Determines how much log the compiler produces."
                <> intercalate " < " [show l | l <- allElements @LogLevel]
            )
      )
  _globalUnsafeIgnorePackageNameConflicts <-
    switch
      ( long "unsafe-ignore-package-name-conflicts"
          <> help "[UNSAFE] Allow the same package to be used with different versions"
      )

  _globalNoCheck <-
    switch
      ( long "dev-no-check"
          <> help "[DEV] Disable input sanity check in Core"
      )
  _globalShowNameIds <-
    switch
      ( long "show-name-ids"
          <> help "[DEV] Show the unique number of each identifier when pretty printing"
      )
  _globalDevShowThreadIds <-
    switch
      ( long "dev-show-thread-ids"
          <> help "[DEV] Show the thread id when compiling a module"
      )
  _globalNumThreads <- parseNumThreads
  return GlobalOptions {..}

parseBuildDir :: Mod OptionFields (Prepath Dir) -> Parser (AppPath Dir)
parseBuildDir m = do
  _pathPath <-
    option
      somePreDirOpt
      ( metavar "BUILD_DIR"
          <> action "directory"
          <> m
      )
  pure AppPath {_pathIsInput = False, ..}

entryPointFromGlobalOptionsPre ::
  (Members '[TaggedLock, EmbedIO] r) =>
  Root ->
  Maybe (Prepath File) ->
  GlobalOptions ->
  Sem r EntryPoint
entryPointFromGlobalOptionsPre root premainFile opts = do
  mainFile <- mapM (prepathToAbsFile (root ^. rootInvokeDir)) premainFile
  entryPointFromGlobalOptions root mainFile opts

entryPointFromGlobalOptions ::
  (Members '[TaggedLock, EmbedIO] r) =>
  Root ->
  Maybe (Path Abs File) ->
  GlobalOptions ->
  Sem r EntryPoint
entryPointFromGlobalOptions root mainFile opts = do
  mabsBuildDir :: Maybe (Path Abs Dir) <- mapM (prepathToAbsDir cwd) optBuildDir
  let def0 = updateEntryPoint mabsBuildDir $ defaultEntryPoint packageBaseId root mainFile
  def <- runReader def0 $ defaultEntryPointIO (root ^. rootRootDir) mainFile
  return $ updateEntryPoint mabsBuildDir def
  where
    optBuildDir :: Maybe (Prepath Dir)
    optBuildDir = fmap (^. pathPath) (opts ^. globalBuildDir)
    cwd = root ^. rootInvokeDir

    updateEntryPoint :: Maybe (Path Abs Dir) -> EntryPoint -> EntryPoint
    updateEntryPoint mabsBuildDir e =
      e
        { _entryPointNoTermination = opts ^. globalNoTermination,
          _entryPointNoPositivity = opts ^. globalNoPositivity,
          _entryPointNoCoverage = opts ^. globalNoCoverage,
          _entryPointNoStdlib = opts ^. globalNoStdlib,
          _entryPointNoCheck = opts ^. globalNoCheck,
          _entryPointGenericOptions = project opts,
          _entryPointBuildDir = maybe (e ^. entryPointBuildDir) (CustomBuildDir . Abs) mabsBuildDir,
          _entryPointOffline = opts ^. globalOffline,
          _entryPointFieldSize = fromMaybe defaultFieldSize $ opts ^. globalFieldSize,
          _entryPointVerify = opts ^. globalVerify
        }

entryPointFromGlobalOptionsNoFile :: (Members '[EmbedIO, TaggedLock] r) => Root -> GlobalOptions -> Sem r EntryPoint
entryPointFromGlobalOptionsNoFile root opts = entryPointFromGlobalOptions root Nothing opts
