{-# LANGUAGE QuasiQuotes #-}

module Commands.Repl where

import Commands.Base hiding
  ( command,
  )
import Commands.Repl.Options
import Control.Exception (throwIO)
import Control.Monad.Except qualified as Except
import Control.Monad.Reader qualified as Reader
import Control.Monad.State.Strict qualified as State
import Control.Monad.Trans.Class (lift)
import Data.String.Interpolate (i, __i)
import Evaluator
import Juvix.Compiler.Concrete.Data.Scope (scopePath)
import Juvix.Compiler.Concrete.Data.ScopedName (absTopModulePath)
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver (runPathResolver)
import Juvix.Compiler.Core qualified as Core
import Juvix.Compiler.Core.Extra.Value
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Info.NoDisplayInfo qualified as Info
import Juvix.Compiler.Core.Pretty qualified as Core
import Juvix.Compiler.Core.Transformation qualified as Core
import Juvix.Compiler.Core.Transformation.DisambiguateNames (disambiguateNames)
import Juvix.Compiler.Internal.Data qualified as Internal
import Juvix.Compiler.Internal.Language qualified as Internal
import Juvix.Compiler.Internal.Pretty qualified as Internal
import Juvix.Compiler.Pipeline.Repl
import Juvix.Compiler.Pipeline.Setup (entrySetup)
import Juvix.Data.Error.GenericError qualified as Error
import Juvix.Extra.Paths
import Juvix.Extra.Stdlib
import Juvix.Extra.Version
import Juvix.Prelude.Pretty
import Juvix.Prelude.Pretty qualified as P
import System.Console.ANSI qualified as Ansi
import System.Console.Haskeline
import System.Console.Repline
import System.Console.Repline qualified as Repline

type ReplS = Reader.ReaderT ReplEnv (State.StateT ReplState (Except.ExceptT JuvixError IO))

type Repl a = HaskelineT ReplS a

data ReplContext = ReplContext
  { _replContextArtifacts :: Artifacts,
    _replContextEntryPoint :: EntryPoint
  }

data ReplEnv = ReplEnv
  { _replRoots :: Roots,
    _replOptions :: ReplOptions
  }

data ReplState = ReplState
  { _replStateRoots :: Roots,
    _replStateContext :: Maybe ReplContext,
    _replStateGlobalOptions :: GlobalOptions
  }

makeLenses ''ReplState
makeLenses ''ReplContext
makeLenses ''ReplEnv

helpTxt :: MonadIO m => m ()
helpTxt =
  liftIO
    ( putStrLn
        [__i|
  EXPRESSION                Evaluate an expression in the context of the currently loaded module
  :help                     Print help text and describe options
  :load       FILE          Load a file into the REPL
  :reload                   Reload the currently loaded file
  :type       EXPRESSION    Infer the type of an expression
  :def        IDENTIFIER    Print the definition of the identifier
  :core       EXPRESSION    Translate the expression to JuvixCore
  :multiline                Start a multi-line input. Submit with <Ctrl-D>
  :root                     Print the current project root
  :version                  Display the Juvix version
  :quit                     Exit the REPL
  |]
    )

replDefaultLoc :: Interval
replDefaultLoc = singletonInterval (mkInitialLoc replPath)

replFromJust :: JuvixError -> Maybe a -> Repl a
replFromJust err = maybe (lift (Except.throwError err)) return

replFromEither :: Either JuvixError a -> Repl a
replFromEither = either (lift . Except.throwError) return

replGetContext :: Repl ReplContext
replGetContext = State.gets (^. replStateContext) >>= replFromJust noFileLoadedErr

replError :: AnsiText -> JuvixError
replError msg =
  JuvixError $
    GenericError
      { _genericErrorLoc = replDefaultLoc,
        _genericErrorMessage = msg,
        _genericErrorIntervals = [replDefaultLoc]
      }

noFileLoadedErr :: JuvixError
noFileLoadedErr = replError (AnsiText @Text "No file loaded. Load a file using the `:load FILE` command.")

welcomeMsg :: MonadIO m => m ()
welcomeMsg = liftIO (putStrLn [i|Juvix REPL version #{versionTag}: https://juvix.org. Run :help for help|])

printHelpTxt :: String -> Repl ()
printHelpTxt _ = helpTxt

multilineCmd :: String
multilineCmd = "multiline"

quit :: String -> Repl ()
quit _ = liftIO (throwIO Interrupt)

loadEntryPoint :: EntryPoint -> Repl ()
loadEntryPoint ep = do
  artif <- liftIO (corePipelineIO' ep)
  let newCtx =
        ReplContext
          { _replContextArtifacts = artif,
            _replContextEntryPoint = ep
          }
  State.modify (set replStateContext (Just newCtx))
  let epPath :: Maybe (Path Abs File) = ep ^? entryPointModulePaths . _head
  whenJust epPath $ \path -> liftIO (putStrLn [i|OK loaded: #{toFilePath path}|])

reloadFile :: String -> Repl ()
reloadFile _ = replGetContext >>= loadEntryPoint . (^. replContextEntryPoint)

pSomeFile :: String -> Prepath File
pSomeFile = mkPrepath

loadFile :: Prepath File -> Repl ()
loadFile f = do
  entryPoint <- getReplEntryPoint f
  loadEntryPoint entryPoint

loadDefaultPrelude :: Repl ()
loadDefaultPrelude = whenJustM defaultPreludeEntryPoint $ \e -> do
  root <- Reader.asks (^. replRoots . rootsRootDir)
  -- The following is needed to ensure that the default location of the
  -- standard library exists
  void
    . liftIO
    . runM
    . runFilesIO
    . runError @Text
    . runReader e
    . runPathResolver root
    $ entrySetup
  loadEntryPoint e

getReplEntryPoint :: Prepath File -> Repl EntryPoint
getReplEntryPoint inputFile = do
  roots <- Reader.asks (^. replRoots)
  gopts <- State.gets (^. replStateGlobalOptions)
  liftIO (entryPointFromGlobalOptionsPre roots inputFile gopts)

displayVersion :: String -> Repl ()
displayVersion _ = liftIO (putStrLn versionTag)

replCommand :: ReplOptions -> String -> Repl ()
replCommand opts input = catchAll $ do
  ctx <- replGetContext
  let tab = ctx ^. replContextArtifacts . artifactCoreTable
  evalRes <- compileThenEval ctx input
  whenJust evalRes $ \n ->
    if
        | Info.member Info.kNoDisplayInfo (Core.getInfo n) -> return ()
        | opts ^. replPrintValues ->
            renderOut (Core.ppOut opts (toValue tab n))
        | otherwise -> renderOut (Core.ppOut opts n)
  where
    compileThenEval :: ReplContext -> String -> Repl (Maybe Core.Node)
    compileThenEval ctx s = compileString >>= mapM eval
      where
        artif :: Artifacts
        artif = ctx ^. replContextArtifacts

        eval :: Core.Node -> Repl Core.Node
        eval n = do
          ep <- getReplEntryPoint (mkPrepath (toFilePath replPath))
          let shouldDisambiguate :: Bool
              shouldDisambiguate = not (opts ^. replNoDisambiguate)
          (artif', n') <-
            replFromEither
              . run
              . runReader ep
              . runError @JuvixError
              . runState artif
              . runTransformations shouldDisambiguate (opts ^. replTransformations)
              $ n
          liftIO (doEvalIO' artif' n') >>= replFromEither

        doEvalIO' :: Artifacts -> Core.Node -> IO (Either JuvixError Core.Node)
        doEvalIO' artif' n =
          mapLeft (JuvixError @Core.CoreError)
            <$> doEvalIO False replDefaultLoc (artif' ^. artifactCoreTable) n

        compileString :: Repl (Maybe Core.Node)
        compileString = do
          (artifacts, res) <- liftIO $ compileReplInputIO' ctx (strip (pack s))
          res' <- replFromEither res
          State.modify (over (replStateContext . _Just) (set replContextArtifacts artifacts))
          return res'

core :: String -> Repl ()
core input = do
  ctx <- replGetContext
  opts <- Reader.asks (^. replOptions)
  compileRes <- liftIO (compileReplInputIO' ctx (strip (pack input))) >>= replFromEither . snd
  whenJust compileRes (renderOut . Core.ppOut opts)

printDefinition :: String -> Repl ()
printDefinition input = do
  ctx <- replGetContext
  gopts <- State.gets (^. replStateGlobalOptions)
  compileRes <- liftIO (inferExpressionIO' ctx (strip (pack input)))
  let tbl :: Internal.InfoTable = ctx ^. replContextArtifacts . artifactInternalTypedTable

      getIdentifier :: Internal.Expression -> Maybe Internal.Iden
      getIdentifier = \case
        Internal.ExpressionIden n -> return n
        _ -> Nothing

      printFunction :: Internal.FunctionName -> Repl ()
      printFunction fun = do
        let def :: Internal.FunctionDef = tbl ^?! Internal.infoFunctions . at fun . _Just . Internal.functionInfoDef
        renderOut (Internal.ppOut (project' @GenericOptions gopts) def)

      printInductive :: Internal.InductiveName -> Repl ()
      printInductive ind = do
        let def :: Internal.InductiveDef = tbl ^?! Internal.infoInductives . at ind . _Just . Internal.inductiveInfoDef
        renderOut (Internal.ppOut (project' @GenericOptions gopts) def)

      printAxiom :: Internal.AxiomName -> Repl ()
      printAxiom ax = do
        let def :: Internal.AxiomDef = tbl ^?! Internal.infoAxioms . at ax . _Just . Internal.axiomInfoDef
        renderOut (Internal.ppOut (project' @GenericOptions gopts) def)

      printConstructor :: Internal.ConstructorName -> Repl ()
      printConstructor c = do
        let ind :: Internal.InductiveName = tbl ^?! Internal.infoConstructors . at c . _Just . Internal.constructorInfoInductive
        printInductive ind

  case (^. Internal.typedExpression) <$> compileRes of
    Left err -> lift (printErrorS err)
    Right expr -> do
      let m = getIdentifier (expr)
      case m of
        Nothing -> do
          liftIO (putStrLn ":def expects a single identifier, but got: ")
          renderOut (Internal.ppOut (project' @GenericOptions gopts) expr)
        Just iden -> case iden of
          Internal.IdenAxiom a -> printAxiom a
          Internal.IdenVar {} -> impossible
          Internal.IdenInductive ind -> printInductive ind
          Internal.IdenConstructor c -> printConstructor c
          Internal.IdenFunction fun -> printFunction fun

inferType :: String -> Repl ()
inferType input = do
  ctx <- replGetContext
  gopts <- State.gets (^. replStateGlobalOptions)
  n <- liftIO (inferExpressionIO' ctx (strip (pack input))) >>= replFromEither
  renderOut (Internal.ppOut (project' @GenericOptions gopts) (n ^. Internal.typedType))

replCommands :: [(String, String -> Repl ())]
replCommands = catchable ++ nonCatchable
  where
    nonCatchable :: [(String, String -> Repl ())]
    nonCatchable =
      [ ("quit", quit)
      ]
    catchable :: [(String, String -> Repl ())]
    catchable =
      map
        (second (catchAll .))
        [ ("help", printHelpTxt),
          -- `multiline` is included here for auto-completion purposes only.
          -- `repline`'s `multilineCommand` logic overrides this no-op.
          (multilineCmd, const (return ())),
          ("load", loadFile . pSomeFile),
          ("reload", reloadFile),
          ("root", printRoot),
          ("def", printDefinition),
          ("type", inferType),
          ("version", displayVersion),
          ("core", core)
        ]

catchAll :: Repl () -> Repl ()
catchAll = Repline.dontCrash . catchJuvixError
  where
    catchJuvixError :: Repl () -> Repl ()
    catchJuvixError (HaskelineT m) = HaskelineT (mapInputT_ catchErrorS m)
      where
        catchErrorS :: ReplS () -> ReplS ()
        catchErrorS = (`Except.catchError` printErrorS)

defaultMatcher :: [(String, CompletionFunc ReplS)]
defaultMatcher = [(":load", fileCompleter)]

optsCompleter :: WordCompleter ReplS
optsCompleter n = do
  let names = (":" <>) . fst <$> replCommands
  return (filter (isPrefixOf n) names)

replBanner :: MultiLine -> Repl String
replBanner = \case
  MultiLine -> return "... "
  SingleLine -> do
    mmodulePath <-
      State.gets
        ( ^?
            replStateContext
              . _Just
              . replContextArtifacts
              . artifactMainModuleScope
              . _Just
              . scopePath
              . absTopModulePath
        )
    case mmodulePath of
      Just path -> return [i|#{unpack (P.prettyText path)}> |]
      Nothing -> return "juvix> "

replPrefix :: Maybe Char
replPrefix = Just ':'

replMultilineCommand :: Maybe String
replMultilineCommand = Just multilineCmd

replInitialiser :: Repl ()
replInitialiser = do
  gopts <- State.gets (^. replStateGlobalOptions)
  opts <- Reader.asks (^. replOptions)
  welcomeMsg
  unless
    (opts ^. replNoPrelude || gopts ^. globalNoStdlib)
    (maybe loadDefaultPrelude (loadFile . (^. pathPath)) (opts ^. replInputFile))

replFinaliser :: Repl ExitDecision
replFinaliser = return Exit

replTabComplete :: CompleterStyle ReplS
replTabComplete = Prefix (wordCompleter optsCompleter) defaultMatcher

printRoot :: String -> Repl ()
printRoot _ = do
  r <- State.gets (^. replStateRoots . rootsRootDir)
  liftIO $ putStrLn (pack (toFilePath r))

runCommand :: Members '[Embed IO, App] r => ReplOptions -> Sem r ()
runCommand opts = do
  roots <- askRoots
  let replAction :: ReplS ()
      replAction = do
        evalReplOpts
          ReplOpts
            { prefix = replPrefix,
              multilineCommand = replMultilineCommand,
              initialiser = replInitialiser,
              finaliser = replFinaliser,
              tabComplete = replTabComplete,
              command = replCommand opts,
              options = replCommands,
              banner = replBanner
            }
  globalOptions <- askGlobalOptions
  let env =
        ReplEnv
          { _replRoots = roots,
            _replOptions = opts
          }
      iniState =
        ReplState
          { _replStateRoots = roots,
            _replStateContext = Nothing,
            _replStateGlobalOptions = globalOptions
          }
  e <-
    embed
      . Except.runExceptT
      . (`State.evalStateT` iniState)
      . (`Reader.runReaderT` env)
      $ replAction
  case e of
    Left {} -> error "impossible: uncaught exception"
    Right () -> return ()

-- | If the package contains the stdlib as a dependency, loads the Prelude
defaultPreludeEntryPoint :: Repl (Maybe EntryPoint)
defaultPreludeEntryPoint = do
  opts <- State.gets (^. replStateGlobalOptions)
  roots <- State.gets (^. replStateRoots)
  let buildDir = roots ^. rootsBuildDir
      root = roots ^. rootsRootDir
      pkg = roots ^. rootsPackage
  mstdlibPath <- liftIO (runM (runFilesIO (packageStdlib root buildDir (pkg ^. packageDependencies))))
  case mstdlibPath of
    Just stdlibPath ->
      Just . set entryPointResolverRoot stdlibPath
        <$> liftIO (entryPointFromGlobalOptions roots (stdlibPath <//> preludePath) opts)
    Nothing -> return Nothing

replMakeAbsolute :: SomeBase b -> Repl (Path Abs b)
replMakeAbsolute = \case
  Abs p -> return p
  Rel r -> do
    invokeDir <- State.gets (^. replStateRoots . rootsInvokeDir)
    return (invokeDir <//> r)

inferExpressionIO' :: ReplContext -> Text -> IO (Either JuvixError Internal.TypedExpression)
inferExpressionIO' ctx txt =
  runM
    . evalState (ctx ^. replContextArtifacts)
    . runReader (ctx ^. replContextEntryPoint)
    $ inferExpressionIO replPath txt

compileReplInputIO' :: ReplContext -> Text -> IO (Artifacts, (Either JuvixError (Maybe Core.Node)))
compileReplInputIO' ctx txt =
  runM
    . runState (ctx ^. replContextArtifacts)
    . runReader (ctx ^. replContextEntryPoint)
    $ do
      r <- compileReplInputIO replPath txt
      return (extractNode <$> r)
  where
    extractNode :: ReplPipelineResult -> Maybe Core.Node
    extractNode = \case
      ReplPipelineResultNode n -> Just n
      ReplPipelineResultImport {} -> Nothing
      ReplPipelineResultOpenImport {} -> Nothing

render' :: (P.HasAnsiBackend a, P.HasTextBackend a) => a -> Repl ()
render' t = do
  opts <- State.gets (^. replStateGlobalOptions)
  hasAnsi <- liftIO (Ansi.hSupportsANSIColor stdout)
  liftIO (P.renderIO (not (opts ^. globalNoColors) && hasAnsi) t)

renderOut :: (P.HasAnsiBackend a, P.HasTextBackend a) => a -> Repl ()
renderOut t = render' t >> liftIO (putStrLn "")

printErrorS :: JuvixError -> ReplS ()
printErrorS e = do
  opts <- State.gets (^. replStateGlobalOptions)
  hasAnsi <- liftIO (Ansi.hSupportsANSIColor stderr)
  liftIO $ hPutStrLn stderr $ run (runReader (project' @GenericOptions opts) (Error.render (not (opts ^. globalNoColors) && hasAnsi) False e))

runTransformations ::
  forall r.
  Members '[State Artifacts, Error JuvixError, Reader EntryPoint] r =>
  Bool ->
  [Core.TransformationId] ->
  Core.Node ->
  Sem r Core.Node
runTransformations shouldDisambiguate ts n = runCoreInfoTableBuilderArtifacts $ do
  sym <- addNode n
  applyTransforms shouldDisambiguate ts
  getNode sym
  where
    addNode :: Core.Node -> Sem (Core.InfoTableBuilder ': r) Core.Symbol
    addNode node = do
      sym <- Core.freshSymbol
      Core.registerIdentNode sym node
      -- `n` will get filtered out by the transformations unless it has a
      -- corresponding entry in `infoIdentifiers`
      tab <- Core.getInfoTable
      let name = Core.freshIdentName tab "_repl"
          idenInfo =
            Core.IdentifierInfo
              { _identifierName = name,
                _identifierSymbol = sym,
                _identifierLocation = Nothing,
                _identifierArgsNum = 0,
                _identifierType = Core.mkDynamic',
                _identifierIsExported = False,
                _identifierBuiltin = Nothing,
                _identifierPragmas = mempty
              }
      Core.registerIdent name idenInfo
      return sym

    applyTransforms :: Bool -> [Core.TransformationId] -> Sem (Core.InfoTableBuilder ': r) ()
    applyTransforms shouldDisambiguate' ts' = do
      tab <- Core.getInfoTable
      tab' <- mapReader Core.fromEntryPoint $ Core.applyTransformations ts' tab
      let tab'' =
            if
                | shouldDisambiguate' -> disambiguateNames tab'
                | otherwise -> tab'
      Core.setInfoTable tab''

    getNode :: Core.Symbol -> Sem (Core.InfoTableBuilder ': r) Core.Node
    getNode sym = fromMaybe impossible . flip Core.lookupIdentifierNode' sym <$> Core.getInfoTable
