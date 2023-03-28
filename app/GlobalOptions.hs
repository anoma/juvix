module GlobalOptions
  ( module GlobalOptions,
  )
where

import CommonOptions
import Juvix.Compiler.Abstract.Pretty.Options qualified as Abstract
import Juvix.Compiler.Core.Options qualified as Core
import Juvix.Compiler.Internal.Pretty.Options qualified as Internal
import Juvix.Compiler.Pipeline (EntryPoint (..), defaultEntryPoint)
import Juvix.Data.Error.GenericError qualified as E
import Juvix.Extra.Paths

data GlobalOptions = GlobalOptions
  { _globalNoColors :: Bool,
    _globalShowNameIds :: Bool,
    _globalBuildDir :: Maybe (AppPath Dir),
    _globalOnlyErrors :: Bool,
    _globalNoApe :: Bool,
    _globalStdin :: Bool,
    _globalNoTermination :: Bool,
    _globalNoPositivity :: Bool,
    _globalNoCoverage :: Bool,
    _globalNoStdlib :: Bool,
    _globalUnrollLimit :: Int
  }
  deriving stock (Eq, Show)

makeLenses ''GlobalOptions

instance CanonicalProjection GlobalOptions Internal.Options where
  project g =
    Internal.Options
      { Internal._optShowNameIds = g ^. globalShowNameIds
      }

instance CanonicalProjection GlobalOptions Abstract.Options where
  project g =
    Abstract.defaultOptions
      { Abstract._optShowNameIds = g ^. globalShowNameIds
      }

instance CanonicalProjection GlobalOptions E.GenericOptions where
  project GlobalOptions {..} =
    E.GenericOptions
      { E._showNameIds = _globalShowNameIds,
        E._genericNoApe = _globalNoApe
      }

instance CanonicalProjection GlobalOptions Core.CoreOptions where
  project GlobalOptions {..} =
    Core.CoreOptions
      { Core._optCheckCoverage = not _globalNoCoverage,
        Core._optUnrollLimit = _globalUnrollLimit
      }

defaultGlobalOptions :: GlobalOptions
defaultGlobalOptions =
  GlobalOptions
    { _globalNoColors = False,
      _globalShowNameIds = False,
      _globalOnlyErrors = False,
      _globalNoApe = False,
      _globalNoTermination = False,
      _globalBuildDir = Nothing,
      _globalStdin = False,
      _globalNoPositivity = False,
      _globalNoCoverage = False,
      _globalNoStdlib = False,
      _globalUnrollLimit = defaultUnrollLimit
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
  _globalShowNameIds <-
    switch
      ( long "show-name-ids"
          <> help "Show the unique number of each identifier when pretty printing"
      )
  _globalBuildDir <-
    optional
      ( parseBuildDir
          ( long "internal-build-dir"
              <> help "Directory for compiler internal output"
          )
      )
  _globalNoApe <-
    switch
      ( long "no-format"
          <> help "Disable the new pretty printing algorithm"
      )
  _globalStdin <-
    switch
      ( long "stdin"
          <> help "Read from Stdin"
      )
  _globalOnlyErrors <-
    switch
      ( long "only-errors"
          <> help "Only print errors in a uniform format (used by juvix-mode)"
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
  _globalUnrollLimit <-
    option
      (fromIntegral <$> naturalNumberOpt)
      ( long "unroll"
          <> value defaultUnrollLimit
          <> help ("Recursion unrolling limit (default: " <> show defaultUnrollLimit <> ")")
      )
  return GlobalOptions {..}

parseBuildDir :: Mod OptionFields (SomeBase Dir) -> Parser (AppPath Dir)
parseBuildDir m = do
  _pathPath <-
    option
      someDirOpt
      ( value (Rel relBuildDir)
          <> metavar "BUILD_DIR"
          <> action "directory"
          <> showDefault
          <> m
      )
  pure AppPath {_pathIsInput = False, ..}

entryPointFromGlobalOptions :: Path Abs Dir -> Path Abs File -> GlobalOptions -> EntryPoint
entryPointFromGlobalOptions root mainFile opts =
  (defaultEntryPoint root mainFile)
    { _entryPointNoTermination = opts ^. globalNoTermination,
      _entryPointNoPositivity = opts ^. globalNoPositivity,
      _entryPointNoCoverage = opts ^. globalNoCoverage,
      _entryPointNoStdlib = opts ^. globalNoStdlib,
      _entryPointUnrollLimit = opts ^. globalUnrollLimit,
      _entryPointGenericOptions = project opts
    }
