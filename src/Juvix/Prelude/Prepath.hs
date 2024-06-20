module Juvix.Prelude.Prepath
  ( Prepath,
    prepath,
    mkPrepath,
    prepathToAbsDir,
    fromPreFileOrDir,
    prepathToAbsFile,
    prepathToFilePath,
    preFileFromAbs,
    unsafePrepathToFilePath,
  )
where

import Data.Yaml
import Juvix.Prelude.Base
import Juvix.Prelude.Parsing as P
import Juvix.Prelude.Path
import Juvix.Prelude.Pretty
import System.Directory (getHomeDirectory)
import System.Directory qualified as System
import System.Environment
import Prelude (show)

-- | A file/directory path that may contain environmental variables
newtype Prepath d = Prepath
  { _prepath :: String
  }
  deriving stock (Eq, Data, Generic)

makeLenses ''Prepath

instance Show (Prepath d) where
  show Prepath {..} = _prepath

type PrepathParts = NonEmpty PrepathPart

data PrepathPart
  = PrepathVar String
  | PrepathString String
  | PrepathHome

-- | Paths have the following restrictions:
-- 1. Environment variables are given with the makefile-like syntax $(VAR)
-- 2. The three characters $ ( ) are reserved for the environment variables syntax.
--    They cannot be part of the path.
-- 3. ~ is reserved for $(HOME). I.e. the prepath ~~ will expand to $HOME$HOME.
-- 4. Nested environment variables are not allowed.
-- 5. Paths cannot start with space.
expandPrepath :: (MonadIO m) => Prepath a -> m FilePath
expandPrepath (Prepath p) =
  let e = parseHelper prepathParts p
   in case e of
        Left er -> error er
        Right r -> expandParts r
  where
    prepathParts :: forall e m. (MonadParsec e String m) => m PrepathParts
    prepathParts = P.takeWhileP Nothing isSpace >> some1 prepathPart
      where
        prepathPart :: m PrepathPart
        prepathPart =
          PrepathVar
            <$> evar
            <|> PrepathHome
            <$ home
            <|> PrepathString
            <$> str
          where
            reserved :: [Char]
            reserved = "$()~"

            notReserved :: Char -> Bool
            notReserved = (`notElem` reserved)

            evar :: m String
            evar = do
              P.chunk "$("
              v <- P.takeWhile1P (Just "<EnvVar>") notReserved
              P.chunk ")"
              return v

            home :: m ()
            home = void (P.chunk "~")

            str :: m String
            str = P.takeWhile1P (Just "<Path>") notReserved

expandParts :: forall m. (MonadIO m) => PrepathParts -> m FilePath
expandParts = mconcatMapM fromPart
  where
    fromPart :: PrepathPart -> m String
    fromPart = \case
      PrepathString s -> return s
      PrepathHome -> liftIO getHomeDirectory
      PrepathVar s -> fromMaybe err <$> liftIO (lookupEnv s)
        where
          err = error ("The environment variable " <> pack s <> " is not defined")

mkPrepath :: String -> Prepath d
mkPrepath = Prepath

instance IsString (Prepath d) where
  fromString = mkPrepath

instance ToJSON (Prepath d) where
  toJSON (Prepath p) = toJSON p
  toEncoding (Prepath p) = toEncoding p

instance FromJSON (Prepath d) where
  parseJSON = fmap mkPrepath . parseJSON

instance Pretty (Prepath d) where
  pretty (Prepath p) = pretty p

prepathToAbsFile :: (MonadIO m) => Path Abs Dir -> Prepath File -> m (Path Abs File)
prepathToAbsFile root = fmap absFile . prepathToFilePath root

prepathToAbsDir :: (MonadIO m) => Path Abs Dir -> Prepath Dir -> m (Path Abs Dir)
prepathToAbsDir root = fmap absDir . prepathToFilePath root

prepathToFilePath :: (MonadIO m) => Path Abs Dir -> Prepath d -> m FilePath
prepathToFilePath root pre = do
  expandedPre <- expandPrepath pre
  liftIO (System.canonicalizePath (toFilePath root </> expandedPre))

fromPreFileOrDir :: (MonadIO m, MonadThrow m) => Path Abs Dir -> Prepath FileOrDir -> m (Either (Path Abs File) (Path Abs Dir))
fromPreFileOrDir cwd fp = do
  absPath <- prepathToFilePath cwd fp
  isDirectory <- liftIO (System.doesDirectoryExist absPath)
  if
    | isDirectory -> Right <$> parseAbsDir absPath
    | otherwise -> Left <$> parseAbsFile absPath

preFileFromAbs :: Path Abs File -> Prepath File
preFileFromAbs = mkPrepath . toFilePath

unsafePrepathToFilePath :: Prepath a -> FilePath
unsafePrepathToFilePath (Prepath p) = p
