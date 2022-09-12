module Main (main) where

import App
import CLI
import Commands.Compile qualified as Compile
import Commands.Dev qualified as Dev
import Commands.Dev.Core qualified as Core
import Commands.Html qualified as Html
import Commands.Init qualified as Init
import Control.Exception qualified as IO
import Data.ByteString qualified as ByteString
import Data.Yaml
import Juvix.Compiler.Pipeline
import Juvix.Extra.Paths qualified as Paths
import Juvix.Extra.Version (runDisplayVersion)
import Juvix.Prelude
import Options.Applicative
import System.Environment (getProgName)

findRoot :: GlobalOptions -> IO (FilePath, Package)
findRoot copts = do
  let dir :: Maybe FilePath
      dir = takeDirectory <$> commandFirstFile copts
  whenJust dir setCurrentDirectory
  r <- IO.try go
  case r of
    Left (err :: IO.SomeException) -> do
      putStrLn "Something went wrong when figuring out the root of the project."
      putStrLn (pack (IO.displayException err))
      exitFailure
    Right root -> return root
  where
    possiblePaths :: FilePath -> [FilePath]
    possiblePaths start = takeWhile (/= "/") (aux start)
      where
        aux f = f : aux (takeDirectory f)

    go :: IO (FilePath, Package)
    go = do
      c <- getCurrentDirectory
      l <- findFile (possiblePaths c) Paths.juvixYamlFile
      case l of
        Nothing -> return (c, emptyPackage)
        Just yaml -> do
          bs <- ByteString.readFile yaml
          let isEmpty = ByteString.null bs
          pkg <-
            if
                | isEmpty -> return emptyPackage
                | otherwise -> decodeThrow bs
          return (takeDirectory yaml, pkg)

getEntryPoint :: FilePath -> Package -> GlobalOptions -> Maybe (IO EntryPoint)
getEntryPoint r pkg opts = nonEmpty (opts ^. globalInputFiles) >>= Just <$> entryPoint
  where
    entryPoint :: NonEmpty FilePath -> IO EntryPoint
    entryPoint l
      | opts ^. globalStdin = aux . Just <$> getContents
      | otherwise = return (aux Nothing)
      where
        aux :: Maybe Text -> EntryPoint
        aux _entryPointStdin =
          EntryPoint
            { _entryPointRoot = r,
              _entryPointNoTermination = opts ^. globalNoTermination,
              _entryPointNoPositivity = opts ^. globalNoPositivity,
              _entryPointNoStdlib = opts ^. globalNoStdlib,
              _entryPointPackage = pkg,
              _entryPointModulePaths = l,
              _entryPointGenericOptions = genericFromGlobalOptions opts,
              _entryPointStdin
            }

runCommand :: forall r. Members '[Embed IO, App] r => Command -> Sem r ()
runCommand cmd = do
  globalOpts <- askGlobalOptions
  (root, pkg) <- embed (findRoot globalOpts)
  case cmd of
    (Dev DisplayRoot) -> say (pack root)
    (Dev (Core cmd')) -> Core.runCommand cmd'
    _ -> do
      -- Other commands require an entry point:
      case getEntryPoint root pkg globalOpts of
        Nothing -> printFailureExit "Provide a Juvix file to run this command\nUse --help to see all the options"
        Just ioEntryPoint -> do
          e <- embed ioEntryPoint
          commandHelper e cmd
          where
            commandHelper :: EntryPoint -> Command -> Sem r ()
            commandHelper entryPoint = \case
              Check -> commandHelper entryPoint (Dev (Internal (TypeCheck mempty)))
              Compile localOpts -> Compile.runCommand entryPoint localOpts
              Html localOpts -> Html.runCommand entryPoint localOpts
              Dev dev -> Dev.runCommand entryPoint dev

showHelpText :: ParserPrefs -> IO ()
showHelpText p = do
  progn <- getProgName
  let helpText = parserFailure p descr (ShowHelpText Nothing) []
  let (msg, _) = renderFailure helpText progn
  putStrLn (pack msg)

main :: IO ()
main = do
  let p = prefs showHelpOnEmpty
  (global, cli) <- customExecParser p descr >>= secondM makeAbsPaths
  case cli of
    DisplayVersion -> runDisplayVersion
    DisplayHelp -> showHelpText p
    Command cmd -> runM (runAppIO global (runCommand cmd))
    Doctor opts -> runM (runLogIO (doctor opts))
    Init -> runM (runLogIO Init.init)
