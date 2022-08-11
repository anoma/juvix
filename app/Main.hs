module Main (main) where

import App
import CLI
import Commands.Dev.Termination as Termination
import Control.Exception qualified as IO
import Control.Monad.Extra
import Data.ByteString qualified as ByteString
import Data.HashMap.Strict qualified as HashMap
import Data.Yaml
import Juvix.Compiler.Abstract.Data.InfoTable qualified as Abstract
import Juvix.Compiler.Abstract.Language qualified as Abstract
import Juvix.Compiler.Abstract.Pretty qualified as Abstract
import Juvix.Compiler.Abstract.Translation.FromConcrete qualified as Abstract
import Juvix.Compiler.Backend.C.Translation.FromInternal qualified as MiniC
import Juvix.Compiler.Backend.Haskell.Pretty qualified as MiniHaskell
import Juvix.Compiler.Backend.Haskell.Translation.FromMono qualified as MiniHaskell
import Juvix.Compiler.Backend.Html.Translation.FromTyped qualified as Doc
import Juvix.Compiler.Backend.Html.Translation.FromTyped qualified as Html
import Juvix.Compiler.Concrete.Data.Highlight qualified as Highlight
import Juvix.Compiler.Concrete.Data.InfoTable qualified as Scoper
import Juvix.Compiler.Concrete.Pretty qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromSource qualified as Parser
import Juvix.Compiler.Internal.Pretty qualified as Internal
import Juvix.Compiler.Internal.Translation.FromAbstract qualified as Internal
import Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination qualified as Termination
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Data.Context qualified as InternalArity
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking qualified as InternalTyped
import Juvix.Compiler.Mono.Pretty qualified as Mono
import Juvix.Compiler.Mono.Translation.FromInternal qualified as Mono
import Juvix.Compiler.Pipeline
import Juvix.Extra.Paths qualified as Paths
import Juvix.Extra.Process
import Juvix.Extra.Version (runDisplayVersion)
import Juvix.Prelude hiding (Doc)
import Juvix.Prelude.Pretty hiding (Doc)
import Options.Applicative
import System.Environment (getProgName)
import System.Process qualified as Process
import Text.Show.Pretty hiding (Html)

findRoot :: CommandGlobalOptions -> IO (FilePath, Package)
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

getEntryPoint :: FilePath -> Package -> GlobalOptions -> Maybe EntryPoint
getEntryPoint r pkg opts = nonEmpty (opts ^. globalInputFiles) >>= Just <$> entryPoint
  where
    entryPoint :: NonEmpty FilePath -> EntryPoint
    entryPoint l =
      EntryPoint
        { _entryPointRoot = r,
          _entryPointNoTermination = opts ^. globalNoTermination,
          _entryPointNoPositivity = opts ^. globalNoPositivity,
          _entryPointNoStdlib = opts ^. globalNoStdlib,
          _entryPointPackage = pkg,
          _entryPointModulePaths = l
        }

runCommand :: Members '[Embed IO, App] r => CommandGlobalOptions -> Sem r ()
runCommand cmdWithOpts = do
  let cmd = cmdWithOpts ^. cliCommand
      globalOpts = cmdWithOpts ^. cliGlobalOptions
      toAnsiText' :: forall a. (HasAnsiBackend a, HasTextBackend a) => a -> Text
      toAnsiText' = toAnsiText (not (globalOpts ^. globalNoColors))
  (root, pkg) <- embed (findRoot cmdWithOpts)
  case cmd of
    (Dev DisplayRoot) -> say (pack root)
    _ -> do
      -- Other commands require an entry point:
      case getEntryPoint root pkg globalOpts of
        Nothing -> printFailureExit "Provide a Juvix file to run this command\nUse --help to see all the options"
        Just entryPoint -> commandHelper cmd
          where
            commandHelper = \case
              -- Visible commands
              Check -> commandHelper (Dev (Internal (TypeCheck mempty)))
              Compile localOpts -> do
                miniC <- (^. MiniC.resultCCode) <$> runPipeline (upToMiniC entryPoint)
                let inputFile = entryPoint ^. mainModulePath
                result <- embed (runCompile root inputFile localOpts miniC)
                case result of
                  Left err -> printFailureExit err
                  _ -> return ()
              Html HtmlOptions {..} -> do
                res <- runPipeline (upToScoping entryPoint)
                let m = head (res ^. Scoper.resultModules)
                embed (Html.genHtml Scoper.defaultOptions _htmlRecursive _htmlTheme _htmlOutputDir _htmlPrintMetadata m)
              (Dev cmd') -> case cmd' of
                Highlight -> do
                  res <- runPipelineEither (upToScoping entryPoint)
                  case res of
                    Left err -> say (Highlight.goError (errorIntervals err))
                    Right r -> do
                      let tbl = r ^. Scoper.resultParserTable
                          items = tbl ^. Parser.infoParsedItems
                          names = r ^. (Scoper.resultScoperTable . Scoper.infoNames)
                          inputFile = entryPoint ^. mainModulePath
                          hinput =
                            Highlight.filterInput
                              inputFile
                              Highlight.HighlightInput
                                { _highlightNames = names,
                                  _highlightParsed = items
                                }
                      say (Highlight.go hinput)
                Parse localOpts -> do
                  m <-
                    head . (^. Parser.resultModules)
                      <$> runPipeline (upToParsing entryPoint)
                  if localOpts ^. parseNoPrettyShow then say (show m) else say (pack (ppShow m))
                Scope localOpts -> do
                  l <-
                    (^. Scoper.resultModules)
                      <$> runPipeline
                        (upToScoping entryPoint)
                  forM_ l $ \s -> do
                    renderStdOut (Scoper.ppOut (mkScopePrettyOptions globalOpts localOpts) s)
                Doc localOpts -> do
                  ctx :: InternalTyped.InternalTypedResult <-
                    runPipeline
                      (upToInternalTyped entryPoint)
                  let docDir = localOpts ^. docOutputDir
                  Doc.compile docDir "proj" ctx
                  when (localOpts ^. docOpen) $ case openCmd of
                    Nothing -> say "Could not recognize the 'open' command for your OS"
                    Just opencmd -> embed (void (Process.spawnProcess opencmd [docDir </> Doc.indexFileName]))
                Internal Pretty -> do
                  micro <-
                    head . (^. Internal.resultModules)
                      <$> runPipeline (upToInternal entryPoint)
                  let ppOpts =
                        Internal.defaultOptions
                          { Internal._optShowNameIds = globalOpts ^. globalShowNameIds
                          }
                  App.renderStdOut (Internal.ppOut ppOpts micro)
                Internal Arity -> do
                  micro <- head . (^. InternalArity.resultModules) <$> runPipeline (upToInternalArity entryPoint)
                  App.renderStdOut (Internal.ppOut Internal.defaultOptions micro)
                Internal (TypeCheck localOpts) -> do
                  res <- runPipeline (upToInternalTyped entryPoint)
                  say "Well done! It type checks"
                  when (localOpts ^. microJuvixTypePrint) $ do
                    let ppOpts =
                          Internal.defaultOptions
                            { Internal._optShowNameIds = globalOpts ^. globalShowNameIds
                            }
                        checkedModule = head (res ^. InternalTyped.resultModules)
                    renderStdOut (Internal.ppOut ppOpts checkedModule)
                    newline
                    let typeCalls = Mono.buildTypeCallMap res
                    renderStdOut (Internal.ppOut ppOpts typeCalls)
                    newline
                    let concreteTypeCalls = Mono.collectTypeCalls res
                    renderStdOut (Internal.ppOut ppOpts concreteTypeCalls)
                MonoJuvix -> do
                  let ppOpts =
                        Mono.defaultOptions
                          { Mono._optShowNameIds = globalOpts ^. globalShowNameIds
                          }
                  monojuvix <- head . (^. Mono.resultModules) <$> runPipeline (upToMonoJuvix entryPoint)
                  renderStdOut (Mono.ppOut ppOpts monojuvix)
                MiniHaskell -> do
                  minihaskell <-
                    head . (^. MiniHaskell.resultModules)
                      <$> runPipeline (upToMiniHaskell entryPoint)
                  renderStdOut (MiniHaskell.ppOutDefault minihaskell)
                MiniC -> do
                  miniC <- (^. MiniC.resultCCode) <$> runPipeline (upToMiniC entryPoint)
                  say miniC
                Termination (Calls localOpts@CallsOptions {..}) -> do
                  results <- runPipeline (upToAbstract entryPoint)
                  let topModule = head (results ^. Abstract.resultModules)
                      infotable = results ^. Abstract.resultTable
                      callMap0 = Termination.buildCallMap infotable topModule
                      callMap = case _callsFunctionNameFilter of
                        Nothing -> callMap0
                        Just f -> Termination.filterCallMap f callMap0
                      localOpts' = Termination.callsPrettyOptions globalOpts localOpts
                  renderStdOut (Abstract.ppOut localOpts' callMap)
                  newline
                Termination (CallGraph CallGraphOptions {..}) -> do
                  results <- runPipeline (upToAbstract entryPoint)
                  let topModule = head (results ^. Abstract.resultModules)
                      infotable = results ^. Abstract.resultTable
                      callMap = Termination.buildCallMap infotable topModule
                      localOpts' =
                        Abstract.defaultOptions
                          { Abstract._optShowNameIds = globalOpts ^. globalShowNameIds
                          }
                      completeGraph = Termination.completeCallGraph callMap
                      filteredGraph =
                        maybe
                          completeGraph
                          (`Termination.unsafeFilterGraph` completeGraph)
                          _graphFunctionNameFilter
                      rEdges = Termination.reflexiveEdges filteredGraph
                      recBehav = map Termination.recursiveBehaviour rEdges
                  App.renderStdOut (Abstract.ppOut localOpts' filteredGraph)
                  newline
                  forM_ recBehav $ \r -> do
                    let funName = r ^. Termination.recursiveBehaviourFun
                        funRef = Abstract.FunctionRef funName
                        funInfo =
                          HashMap.lookupDefault
                            impossible
                            funRef
                            (infotable ^. Abstract.infoFunctions)
                        markedTerminating = funInfo ^. (Abstract.functionInfoDef . Abstract.funDefTerminating)
                        ppOpts =
                          Abstract.defaultOptions
                            { Abstract._optShowNameIds = globalOpts ^. globalShowNameIds
                            }
                        n = toAnsiText' (Abstract.ppOut ppOpts funName)
                    App.renderStdOut (Abstract.ppOut localOpts' r)
                    newline
                    if
                        | markedTerminating ->
                            printSuccessExit (n <> " Terminates by assumption")
                        | otherwise ->
                            case Termination.findOrder r of
                              Nothing ->
                                printFailureExit (n <> " Fails the termination checking")
                              Just (Termination.LexOrder k) ->
                                printSuccessExit (n <> " Terminates with order " <> show (toList k))
                _ -> impossible

showHelpText :: ParserPrefs -> IO ()
showHelpText p = do
  progn <- getProgName
  let helpText = parserFailure p descr (ShowHelpText Nothing) []
  let (msg, _) = renderFailure helpText progn
  putStrLn (pack msg)

main :: IO ()
main = do
  let p = prefs showHelpOnEmpty
  cli <- customExecParser p descr >>= makeAbsPaths
  case cli of
    DisplayVersion -> runDisplayVersion
    DisplayHelp -> showHelpText p
    Command cmd -> runM (runAppIO (cmd ^. cliGlobalOptions) (runCommand cmd))
    Doctor opts -> runM (runLogIO (doctor opts))
