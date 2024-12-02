module Commands.Init where

import App
import Commands.Extra.Package
import Commands.Init.Options
import Data.Text qualified as Text
import Data.Versions
import Juvix.Compiler.Pipeline.Package
import Juvix.Compiler.Pipeline.Package.IO
import Juvix.Data.Effect.Fail.Extra qualified as Fail
import Juvix.Data.Effect.TaggedLock
import Juvix.Extra.Paths
import Juvix.Prelude
import Juvix.Prelude.Pretty
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P

type Err = Text

parse :: Parsec Void Text a -> Text -> Either Err a
parse p t = mapLeft ppErr (P.runParser p "<stdin>" t)

ppErr :: P.ParseErrorBundle Text Void -> Text
ppErr = pack . errorBundlePretty

init :: forall r. (Members '[EmbedIO, App] r) => InitOptions -> Sem r ()
init opts = do
  checkNotInProject
  cwd <- getCurrentDir
  when isInteractive (renderStdOutLn ("creating " <> pack (toFilePath packageFilePath)))
  if
      | opts ^. initOptionsBasic -> writeBasicPackage cwd
      | otherwise -> do
          pkg <-
            if
                | isInteractive -> do
                    renderStdOutLn @Text "✨ Your next Juvix adventure is about to begin! ✨"
                    renderStdOutLn @Text "I will help you set it up"
                    getPackage
                | otherwise -> do
                    projectName <- getDefaultProjectName
                    let emptyPkg = emptyPackage DefaultBuildDir (cwd <//> packageFilePath)
                    return $ case projectName of
                      Nothing -> emptyPkg
                      Just n -> emptyPkg {_packageName = n}
          writePackageFile cwd pkg
  checkPackage
  when isInteractive (renderStdOutLn @Text "you are all set")
  where
    isInteractive :: Bool
    isInteractive = opts ^. initOptionsInteractive

checkNotInProject :: forall r. (Members '[EmbedIO, App] r) => Sem r ()
checkNotInProject =
  whenM (orM [doesFileExist juvixYamlFile, doesFileExist packageFilePath]) err
  where
    err :: Sem r ()
    err = do
      renderStdOutLn @Text "You are already in a Juvix project"
      exitFailure

checkPackage :: forall r. (Members '[EmbedIO, App] r) => Sem r ()
checkPackage = do
  cwd <- getCurrentDir
  ep <- runError @JuvixError (runTaggedLockPermissive (loadPackageFileIO cwd DefaultBuildDir))
  case ep of
    Left {} -> do
      renderStdOutLn @Text "Package.juvix is invalid. Please raise an issue at https://github.com/anoma/juvix/issues"
      exitFailure
    Right {} -> return ()

getPackage :: forall r. (Members '[EmbedIO, App] r) => Sem r Package
getPackage = do
  tproj <- getProjName
  renderStdOutLn @Text "Write the version of your project [leave empty for 0.0.0]"
  tversion :: SemVer <- getVersion
  cwd <- getCurrentDir
  return
    Package
      { _packageName = tproj,
        _packageVersion = tversion,
        _packageBuildDir = Nothing,
        _packageMain = Nothing,
        _packageDependencies = [defaultStdlibDep DefaultBuildDir],
        _packageFile = cwd <//> juvixYamlFile,
        _packageLockfile = Nothing
      }

getDefaultProjectName :: (Member EmbedIO r) => Sem r (Maybe Text)
getDefaultProjectName = runFail $ do
  dir <- map toLower . dropTrailingPathSeparator . toFilePath . dirname <$> getCurrentDir
  Fail.fromRight (parse projectNameParser (pack dir))

getProjName :: forall r. (Members '[EmbedIO, App] r) => Sem r Text
getProjName = do
  d <- getDefaultProjectName
  let defMsg :: Text
      defMsg = case d of
        Nothing -> mempty
        Just d' -> " [leave empty for '" <> d' <> "']"
  renderStdOutLn
    ( "Write the name of your project"
        <> defMsg
        <> " (lower case letters, numbers and dashes are allowed):"
    )
  readName d
  where
    readName :: Maybe Text -> Sem r Text
    readName def = go
      where
        go :: Sem r Text
        go = do
          txt <- getLine
          if
              | Text.null txt, Just def' <- def -> return def'
              | otherwise ->
                  case parse projectNameParser txt of
                    Right p
                      | Text.length p <= projextNameMaxLength -> return p
                      | otherwise -> do
                          renderStdOutLn ("The project name cannot exceed " <> prettyText projextNameMaxLength <> " characters")
                          retry
                    Left err -> do
                      renderStdOut err
                      retry
          where
            retry :: Sem r Text
            retry = do
              tryAgain
              go

tryAgain :: (Members '[App] r) => Sem r ()
tryAgain = renderStdOutLn @Text "Please, try again:"

getVersion :: forall r. (Members '[App, EmbedIO] r) => Sem r SemVer
getVersion = do
  txt <- getLine
  if
      | Text.null txt -> return defaultVersion
      | otherwise -> case parse semver' txt of
          Right r -> return r
          Left err -> do
            renderStdOutLn err
            renderStdOutLn @Text "The version must follow the 'Semantic Versioning 2.0.0' specification"
            retry
  where
    retry :: Sem r SemVer
    retry = do
      tryAgain
      getVersion

projextNameMaxLength :: Int
projextNameMaxLength = 100

projectNameParser :: Parsec Void Text Text
projectNameParser = do
  h <- P.satisfy validFirstChar
  t <- P.takeWhileP (Just "project name character") validChar
  P.hspace
  P.eof
  return (Text.cons h t)
  where
    validFirstChar :: Char -> Bool
    validFirstChar c =
      isAscii c
        && (isLower c || isNumber c)
    validChar :: Char -> Bool
    validChar c = c == '-' || validFirstChar c
