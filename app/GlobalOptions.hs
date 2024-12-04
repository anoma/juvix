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
  { _globalNoColors :: Bool,
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
    _globalUnrollLimit :: Int,
    _globalNumThreads :: NumThreads,
    _globalFieldSize :: Maybe Natural,
    _globalOffline :: Bool,
    _globalLogLevel :: LogLevel,
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
        Core._optUnrollLimit = _globalUnrollLimit,
        Core._optFieldSize = fromMaybe defaultFieldSize _globalFieldSize,
        Core._optOptimizationLevel = defaultOptimizationLevel,
        Core._optInliningDepth = defaultInliningDepth
      }

defaultGlobalOptions :: GlobalOptions
defaultGlobalOptions =
  GlobalOptions
    { _globalNoColors = False,
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
      _globalUnrollLimit = defaultUnrollLimit,
      _globalFieldSize = Nothing,
      _globalDevShowThreadIds = False,
      _globalOffline = False
    }

-- | Get a parser for global flags which can be hidden or not depending on
-- the input boolean
parseGlobalFlags :: Parser GlobalOptions
parseGlobalFlags = do
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
  _globalNoCheck <-
    switch
      ( long "no-check"
          <> help "Disable input sanity check in Core"
      )
  _globalFieldSize <-
    option
      fieldSizeOpt
      ( long "field-size"
          <> value Nothing
          <> help "Field type size [cairo,small,11] (default: small)"
      )
  _globalUnrollLimit <-
    option
      (fromIntegral <$> naturalNumberOpt)
      ( long "unroll"
          <> value defaultUnrollLimit
          <> help ("Recursion unrolling limit (default: " <> show defaultUnrollLimit <> ")")
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
  mabsBuildDir :: Maybe (Path Abs Dir) <- liftIO (mapM (prepathToAbsDir cwd) optBuildDir)
  def <- defaultEntryPointIO (root ^. rootRootDir) mainFile
  return
    def
      { _entryPointNoTermination = opts ^. globalNoTermination,
        _entryPointNoPositivity = opts ^. globalNoPositivity,
        _entryPointNoCoverage = opts ^. globalNoCoverage,
        _entryPointNoStdlib = opts ^. globalNoStdlib,
        _entryPointNoCheck = opts ^. globalNoCheck,
        _entryPointUnrollLimit = opts ^. globalUnrollLimit,
        _entryPointGenericOptions = project opts,
        _entryPointBuildDir = maybe (def ^. entryPointBuildDir) (CustomBuildDir . Abs) mabsBuildDir,
        _entryPointOffline = opts ^. globalOffline,
        _entryPointFieldSize = fromMaybe defaultFieldSize $ opts ^. globalFieldSize
      }
  where
    optBuildDir :: Maybe (Prepath Dir)
    optBuildDir = fmap (^. pathPath) (opts ^. globalBuildDir)
    cwd = root ^. rootInvokeDir

entryPointFromGlobalOptionsNoFile :: (Members '[EmbedIO, TaggedLock] r, MonadIO (Sem r)) => Root -> GlobalOptions -> Sem r EntryPoint
entryPointFromGlobalOptionsNoFile root opts = do
  mabsBuildDir :: Maybe (Path Abs Dir) <- mapM (prepathToAbsDir cwd) optBuildDir
  def <- defaultEntryPointIO (root ^. rootRootDir) Nothing
  return
    def
      { _entryPointNoTermination = opts ^. globalNoTermination,
        _entryPointNoPositivity = opts ^. globalNoPositivity,
        _entryPointNoCoverage = opts ^. globalNoCoverage,
        _entryPointNoStdlib = opts ^. globalNoStdlib,
        _entryPointNoCheck = opts ^. globalNoCheck,
        _entryPointUnrollLimit = opts ^. globalUnrollLimit,
        _entryPointGenericOptions = project opts,
        _entryPointBuildDir = maybe (def ^. entryPointBuildDir) (CustomBuildDir . Abs) mabsBuildDir,
        _entryPointOffline = opts ^. globalOffline,
        _entryPointFieldSize = fromMaybe defaultFieldSize $ opts ^. globalFieldSize
      }
  where
    optBuildDir :: Maybe (Prepath Dir)
    optBuildDir = fmap (^. pathPath) (opts ^. globalBuildDir)
    cwd = root ^. rootInvokeDir
