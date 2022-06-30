module Main (main) where

import App
import CLI
import Commands.Termination as Termination
import Control.Exception qualified as IO
import Control.Monad.Extra
import Data.HashMap.Strict qualified as HashMap
import MiniJuvix.Pipeline
import MiniJuvix.Prelude hiding (Doc)
import MiniJuvix.Prelude.Pretty hiding (Doc)
import MiniJuvix.Syntax.Abstract.InfoTable qualified as Abstract
import MiniJuvix.Syntax.Abstract.Language qualified as Abstract
import MiniJuvix.Syntax.Abstract.Pretty qualified as Abstract
import MiniJuvix.Syntax.Concrete.Parser qualified as Parser
import MiniJuvix.Syntax.Concrete.Scoped.Highlight qualified as Highlight
import MiniJuvix.Syntax.Concrete.Scoped.InfoTable qualified as Scoper
import MiniJuvix.Syntax.Concrete.Scoped.Pretty qualified as Scoper
import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Html
import MiniJuvix.Syntax.Concrete.Scoped.Scoper qualified as Scoper
import MiniJuvix.Syntax.MicroJuvix.MicroJuvixArityResult qualified as MicroArity
import MiniJuvix.Syntax.MicroJuvix.Pretty qualified as Micro
import MiniJuvix.Syntax.MicroJuvix.TypeChecker qualified as MicroTyped
import MiniJuvix.Syntax.MiniHaskell.Pretty qualified as MiniHaskell
import MiniJuvix.Syntax.MonoJuvix.Pretty qualified as Mono
import MiniJuvix.Termination qualified as Termination
import MiniJuvix.Translation.AbstractToMicroJuvix qualified as Micro
import MiniJuvix.Translation.MicroJuvixToMonoJuvix qualified as Mono
import MiniJuvix.Translation.MonoJuvixToMiniC qualified as MiniC
import MiniJuvix.Translation.MonoJuvixToMiniHaskell qualified as MiniHaskell
import MiniJuvix.Translation.ScopedToAbstract qualified as Abstract
import MiniJuvix.Utils.Version (runDisplayVersion)
import Options.Applicative
import System.Environment (getProgName)
import Text.Show.Pretty hiding (Html)

minijuvixYamlFile :: FilePath
minijuvixYamlFile = "minijuvix.yaml"

findRoot :: CommandGlobalOptions -> IO FilePath
findRoot copts = do
  let dir :: Maybe FilePath
      dir = takeDirectory <$> commandFirstFile copts
  whenJust dir setCurrentDirectory
  r <- IO.try go :: IO (Either IO.SomeException FilePath)
  case r of
    Left err -> do
      putStrLn "Something went wrong when figuring out the root of the project."
      putStrLn (pack (IO.displayException err))
      cur <- getCurrentDirectory
      putStrLn ("I will try to use the current directory: " <> pack cur)
      return cur
    Right root -> return root
  where
    possiblePaths :: FilePath -> [FilePath]
    possiblePaths start = takeWhile (/= "/") (aux start)
      where
        aux f = f : aux (takeDirectory f)

    go :: IO FilePath
    go = do
      c <- getCurrentDirectory
      l <- findFile (possiblePaths c) minijuvixYamlFile
      case l of
        Nothing -> return c
        Just yaml -> return (takeDirectory yaml)

getEntryPoint :: FilePath -> GlobalOptions -> Maybe EntryPoint
getEntryPoint r opts = nonEmpty (opts ^. globalInputFiles) >>= Just <$> entryPoint
  where
    entryPoint :: NonEmpty FilePath -> EntryPoint
    entryPoint l =
      EntryPoint
        { _entryPointRoot = r,
          _entryPointNoTermination = opts ^. globalNoTermination,
          _entryPointNoStdlib = opts ^. globalNoStdlib,
          _entryPointModulePaths = l
        }

runCommand :: Members '[Embed IO, App] r => CommandGlobalOptions -> Sem r ()
runCommand cmdWithOpts = do
  let cmd = cmdWithOpts ^. cliCommand
      globalOpts = cmdWithOpts ^. cliGlobalOptions
      toAnsiText' :: forall a. (HasAnsiBackend a, HasTextBackend a) => a -> Text
      toAnsiText' = toAnsiText (not (globalOpts ^. globalNoColors))
  root <- embed (findRoot cmdWithOpts)
  case cmd of
    DisplayRoot -> say (pack root)
    _ -> do
      -- Other commands require an entry point:
      case getEntryPoint root globalOpts of
        Nothing -> printFailureExit "Provide a MiniJuvix file to run this command\nUse --help to see all the options"
        Just entryPoint -> do
          case cmd of
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
            Html HtmlOptions {..} -> do
              res <- runPipeline (upToScoping entryPoint)
              let m = head (res ^. Scoper.resultModules)
              embed (genHtml Scoper.defaultOptions _htmlRecursive _htmlTheme _htmlOutputDir _htmlPrintMetadata m)
            MicroJuvix Pretty -> do
              micro <-
                head . (^. Micro.resultModules)
                  <$> runPipeline (upToMicroJuvix entryPoint)
              let ppOpts =
                    Micro.defaultOptions
                      { Micro._optShowNameIds = globalOpts ^. globalShowNameIds
                      }
              App.renderStdOut (Micro.ppOut ppOpts micro)
            MicroJuvix Arity -> do
              micro <- head . (^. MicroArity.resultModules) <$> runPipeline (upToMicroJuvixArity entryPoint)
              App.renderStdOut (Micro.ppOut Micro.defaultOptions micro)
            MicroJuvix (TypeCheck localOpts) -> do
              res <- runPipeline (upToMicroJuvixTyped entryPoint)
              say "Well done! It type checks"
              when (localOpts ^. microJuvixTypePrint) $ do
                let ppOpts =
                      Micro.defaultOptions
                        { Micro._optShowNameIds = globalOpts ^. globalShowNameIds
                        }
                    checkedModule = head (res ^. MicroTyped.resultModules)
                renderStdOut (Micro.ppOut ppOpts checkedModule)
                newline
                let typeCalls = Mono.buildTypeCallMap res
                renderStdOut (Micro.ppOut ppOpts typeCalls)
                newline
                let concreteTypeCalls = Mono.collectTypeCalls res
                renderStdOut (Micro.ppOut ppOpts concreteTypeCalls)
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
            Compile localOpts -> do
              miniC <- (^. MiniC.resultCCode) <$> runPipeline (upToMiniC entryPoint)
              let inputFile = entryPoint ^. mainModulePath
              result <- embed (runCompile root inputFile localOpts miniC)
              case result of
                Left err -> printFailureExit err
                _ -> return ()
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
