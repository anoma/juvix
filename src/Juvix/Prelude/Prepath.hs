module Juvix.Prelude.Prepath
  ( Prepath,
    prepath,
    mkPrepath,
    prepathToAbsDir,
    fromPreFileOrDir,
    prepathToAbsFile,
    prepathToFilePath,
    preFileFromAbs,
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

-- | A file/directory path that may contain environmental variables
newtype Prepath d = Prepath {_prepath :: String}
  deriving stock (Show, Eq, Data, Generic)

makeLenses ''Prepath

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
expandPrepath :: Prepath a -> IO FilePath
expandPrepath (Prepath p) =
  let e = parseHelper prepathParts p
   in case e of
        Left er -> error er
        Right r -> expandParts r
  where
    prepathParts :: forall e m. MonadParsec e String m => m PrepathParts
    prepathParts = P.takeWhileP Nothing isSpace >> some1 prepathPart
      where
        prepathPart :: m PrepathPart
        prepathPart =
          PrepathVar <$> evar
            <|> PrepathString <$> str
          where
            reserved :: [Char]
            reserved = "$()"
            notReserved :: Char -> Bool
            notReserved = (`notElem` reserved)
            evar :: m String
            evar = do
              P.chunk "$("
              v <- P.takeWhile1P (Just "<EnvVar>") notReserved
              P.chunk ")"
              return v
            str :: m String
            str = P.takeWhile1P (Just "<Path>") notReserved

expandParts :: PrepathParts -> IO FilePath
expandParts = mconcatMapM fromPart
  where
    fromPart :: PrepathPart -> IO String
    fromPart = \case
      PrepathString s -> return s
      PrepathHome -> getHomeDirectory
      PrepathVar s -> fromMaybe err <$> lookupEnv s
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

prepathToAbsFile :: Path Abs Dir -> Prepath File -> IO (Path Abs File)
prepathToAbsFile root = fmap absFile . prepathToFilePath root

prepathToAbsDir :: Path Abs Dir -> Prepath Dir -> IO (Path Abs Dir)
prepathToAbsDir root = fmap absDir . prepathToFilePath root

prepathToFilePath :: Path Abs Dir -> Prepath d -> IO FilePath
prepathToFilePath root pre =
  withCurrentDir root $
    expandPrepath pre >>= System.canonicalizePath

fromPreFileOrDir :: Path Abs Dir -> Prepath FileOrDir -> IO (Either (Path Abs File) (Path Abs Dir))
fromPreFileOrDir cwd fp = do
  absPath <- prepathToFilePath cwd fp
  isDirectory <- System.doesDirectoryExist absPath
  if
      | isDirectory -> Right <$> parseAbsDir absPath
      | otherwise -> Left <$> parseAbsFile absPath

preFileFromAbs :: Path Abs File -> Prepath File
preFileFromAbs = mkPrepath . toFilePath
