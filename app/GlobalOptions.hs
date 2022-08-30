module GlobalOptions
  ( module GlobalOptions,
  )
where

import Commands.Extra
import Juvix.Data.Error.GenericError qualified as E
import Juvix.Prelude
import Options.Applicative hiding (hidden)

data GlobalOptions = GlobalOptions
  { _globalNoColors :: Bool,
    _globalShowNameIds :: Bool,
    _globalOnlyErrors :: Bool,
    _globalStdin :: Bool,
    _globalNoTermination :: Bool,
    _globalNoPositivity :: Bool,
    _globalNoStdlib :: Bool,
    _globalInputFiles :: [FilePath]
  }
  deriving stock (Eq, Show)

makeLenses ''GlobalOptions

defaultGlobalOptions :: GlobalOptions
defaultGlobalOptions =
  GlobalOptions
    { _globalNoColors = False,
      _globalShowNameIds = False,
      _globalOnlyErrors = False,
      _globalNoTermination = False,
      _globalStdin = False,
      _globalNoPositivity = False,
      _globalNoStdlib = False,
      _globalInputFiles = []
    }

instance Semigroup GlobalOptions where
  o1 <> o2 =
    GlobalOptions
      { _globalNoColors = o1 ^. globalNoColors || o2 ^. globalNoColors,
        _globalShowNameIds = o1 ^. globalShowNameIds || o2 ^. globalShowNameIds,
        _globalOnlyErrors = o1 ^. globalOnlyErrors || o2 ^. globalOnlyErrors,
        _globalStdin = o1 ^. globalStdin || o2 ^. globalStdin,
        _globalNoTermination = o1 ^. globalNoTermination || o2 ^. globalNoTermination,
        _globalNoPositivity = o1 ^. globalNoPositivity || o2 ^. globalNoPositivity,
        _globalNoStdlib = o1 ^. globalNoStdlib || o2 ^. globalNoStdlib,
        _globalInputFiles = o1 ^. globalInputFiles ++ o2 ^. globalInputFiles
      }

instance Monoid GlobalOptions where
  mempty = defaultGlobalOptions
  mappend = (<>)

-- | Get a parser for global flags which can be hidden or not depending on
-- the input boolean
parseGlobalFlags :: Bool -> Parser GlobalOptions
parseGlobalFlags b = do
  _globalNoColors <-
    switch
      ( long "no-colors"
          <> help "Disable ANSI formatting"
          <> hidden b
      )
  _globalShowNameIds <-
    switch
      ( long "show-name-ids"
          <> help "Show the unique number of each identifier when pretty printing"
          <> hidden b
      )
  _globalStdin <-
    switch
      ( long "stdin"
          <> help "Read from Stdin"
          <> hidden b
      )
  _globalOnlyErrors <-
    switch
      ( long "only-errors"
          <> help "Only print errors in a uniform format (used by juvix-mode)"
          <> hidden b
      )
  _globalNoTermination <-
    switch
      ( long "no-termination"
          <> help "Disable termination checking"
          <> hidden b
      )
  _globalNoPositivity <-
    switch
      ( long "no-positivity"
          <> help "Disable positivity checking for inductive types"
          <> hidden b
      )
  _globalNoStdlib <-
    switch
      ( long "no-stdlib"
          <> help "Do not use the standard library"
          <> hidden b
      )
  return GlobalOptions {_globalInputFiles = [], ..}

parseGlobalOptions :: Bool -> Parser GlobalOptions
parseGlobalOptions b = do
  opts <- parseGlobalFlags b
  files <- parserInputFiles
  return opts {_globalInputFiles = files}

genericFromGlobalOptions :: GlobalOptions -> E.GenericOptions
genericFromGlobalOptions GlobalOptions {..} = E.GenericOptions {E._showNameIds = _globalShowNameIds}
