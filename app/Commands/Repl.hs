{-# LANGUAGE QuasiQuotes #-}

module Commands.Repl where

import Commands.Base hiding
  ( command,
  )
import Commands.Repl.Base
import Commands.Repl.Options
import Control.Exception (throwIO)
import Control.Monad.Except qualified as Except
import Control.Monad.Reader qualified as Reader
import Control.Monad.State.Strict qualified as State
import Control.Monad.Trans.Class (lift)
import Data.String.Interpolate (i, __i)
import Evaluator
import Juvix.Compiler.Concrete.Data.InfoTable qualified as Scoped
import Juvix.Compiler.Concrete.Data.Scope (scopePath)
import Juvix.Compiler.Concrete.Data.ScopedName (absTopModulePath)
import Juvix.Compiler.Concrete.Data.ScopedName qualified as Scoped
import Juvix.Compiler.Concrete.Language qualified as Concrete
import Juvix.Compiler.Concrete.Pretty qualified as Concrete
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver (runPathResolver)
import Juvix.Compiler.Core qualified as Core
import Juvix.Compiler.Core.Extra.Value
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Info.NoDisplayInfo qualified as Info
import Juvix.Compiler.Core.Pretty qualified as Core
import Juvix.Compiler.Core.Transformation qualified as Core
import Juvix.Compiler.Core.Transformation.DisambiguateNames (disambiguateNames)
import Juvix.Compiler.Internal.Language qualified as Internal
import Juvix.Compiler.Internal.Pretty qualified as Internal
import Juvix.Compiler.Pipeline.Repl
import Juvix.Compiler.Pipeline.Setup (entrySetup)
import Juvix.Data.Error.GenericError qualified as Error
import Juvix.Data.NameKind
import Juvix.Extra.Paths
import Juvix.Extra.Stdlib
import Juvix.Extra.Version
import Juvix.Prelude.Pretty
import Juvix.Prelude.Pretty qualified as P
import System.Console.ANSI qualified as Ansi
import System.Console.Haskeline
import System.Console.Repline
import System.Console.Repline qualified as Repline

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
  :doc        IDENTIFIER    Print the documentation of the identifier
  :core       EXPRESSION    Translate the expression to JuvixCore
  :multiline                Start a multi-line input. Submit with <Ctrl-D>
  :root                     Print the current project root
  :version                  Display the Juvix version
  :quit                     Exit the REPL
  |]
    )

replDefaultLoc :: Interval
replDefaultLoc = singletonInterval (mkInitialLoc replPath)

replFromJust :: Repl a -> Maybe a -> Repl a
replFromJust err = maybe err return

replFromEither :: Either JuvixError a -> Repl a
replFromEither = either (lift . Except.throwError) return

replGetContext :: Repl ReplContext
replGetContext = State.gets (^. replStateContext) >>= replFromJust noFileLoadedErr

replError :: AnsiText -> Repl a
replError msg =
  lift
    . Except.throwError
    . JuvixError
    $ GenericError
      { _genericErrorLoc = replDefaultLoc,
        _genericErrorMessage = msg,
        _genericErrorIntervals = [replDefaultLoc]
      }

noFileLoadedErr :: Repl a
noFileLoadedErr = replError (mkAnsiText @Text "No file loaded. Load a file using the `:load FILE` command.")

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
            renderOutLn (Core.ppOut opts (toValue tab n))
        | otherwise -> renderOutLn (Core.ppOut opts n)
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
  whenJust compileRes (renderOutLn . Core.ppOut opts)

ppConcrete :: Concrete.PrettyCode a => a -> Repl AnsiText
ppConcrete a = do
  gopts <- State.gets (^. replStateGlobalOptions)
  let popts :: GenericOptions = project' gopts
  return (Concrete.ppOut popts a)

printConcrete :: Concrete.PrettyCode a => a -> Repl ()
printConcrete = ppConcrete >=> renderOut

printConcreteLn :: Concrete.PrettyCode a => a -> Repl ()
printConcreteLn = ppConcrete >=> renderOutLn

replParseIdentifiers :: String -> Repl (NonEmpty Concrete.ScopedIden)
replParseIdentifiers input =
  replExpressionUpToScopedAtoms (strip (pack input))
    >>= getIdentifiers
  where
    getIdentifiers :: Concrete.ExpressionAtoms 'Concrete.Scoped -> Repl (NonEmpty Concrete.ScopedIden)
    getIdentifiers as = mapM getIdentifier (as ^. Concrete.expressionAtoms)
      where
        getIdentifier :: Concrete.ExpressionAtom 'Concrete.Scoped -> Repl (Concrete.ScopedIden)
        getIdentifier = \case
          Concrete.AtomIdentifier a -> return a
          Concrete.AtomParens p
            | Concrete.ExpressionIdentifier a <- p -> return a
            | Concrete.ExpressionParensIdentifier a <- p -> return a
          _ -> err
          where
            err :: Repl a
            err = replError (mkAnsiText @Text ":def expects one or more identifiers")

printDocumentation :: String -> Repl ()
printDocumentation = replParseIdentifiers >=> printIdentifiers
  where
    printIdentifiers :: NonEmpty Concrete.ScopedIden -> Repl ()
    printIdentifiers (d :| ds) = do
      printIdentifier d
      whenJust (nonEmpty ds) $ \ds' -> replNewline >> printIdentifiers ds'
      where
        getInfoTable :: Repl Scoped.InfoTable
        getInfoTable = (^. replContextArtifacts . artifactScopeTable) <$> replGetContext

        printIdentifier :: Concrete.ScopedIden -> Repl ()
        printIdentifier s = do
          mdoc <- case s of
            Concrete.ScopedAxiom a -> getDocAxiom (a ^. Concrete.axiomRefName . Scoped.nameId)
            Concrete.ScopedInductive a -> getDocInductive (a ^. Concrete.inductiveRefName . Scoped.nameId)
            Concrete.ScopedVar {} -> return Nothing
            Concrete.ScopedFunction f -> getDocFunction (f ^. Concrete.functionRefName . Scoped.nameId)
            Concrete.ScopedConstructor c -> getDocConstructor (c ^. Concrete.constructorRefName . Scoped.nameId)
          printDoc mdoc
          where
            printDoc :: Maybe (Concrete.Judoc 'Concrete.Scoped) -> Repl ()
            printDoc = \case
              Nothing -> do
                s' <- ppConcrete s
                renderOut (mkAnsiText @Text "No documentation available for ")
                renderOutLn s'
              Just ju -> printConcrete ju

            getDocFunction :: Scoped.NameId -> Repl (Maybe (Concrete.Judoc 'Concrete.Scoped))
            getDocFunction fun = do
              tbl :: Scoped.InfoTable <- getInfoTable
              let def :: Scoped.FunctionInfo = tbl ^?! Scoped.infoFunctions . at fun . _Just
              return (def ^. Scoped.functionInfoType . Concrete.sigDoc)

            getDocInductive :: Scoped.NameId -> Repl (Maybe (Concrete.Judoc 'Concrete.Scoped))
            getDocInductive ind = do
              tbl :: Scoped.InfoTable <- (^. replContextArtifacts . artifactScopeTable) <$> replGetContext
              let def :: Concrete.InductiveDef 'Concrete.Scoped = tbl ^?! Scoped.infoInductives . at ind . _Just . Scoped.inductiveInfoDef
              return (def ^. Concrete.inductiveDoc)

            getDocAxiom :: Scoped.NameId -> Repl (Maybe (Concrete.Judoc 'Concrete.Scoped))
            getDocAxiom ax = do
              tbl :: Scoped.InfoTable <- (^. replContextArtifacts . artifactScopeTable) <$> replGetContext
              let def :: Concrete.AxiomDef 'Concrete.Scoped = tbl ^?! Scoped.infoAxioms . at ax . _Just . Scoped.axiomInfoDef
              return (def ^. Concrete.axiomDoc)

            getDocConstructor :: Scoped.NameId -> Repl (Maybe (Concrete.Judoc 'Concrete.Scoped))
            getDocConstructor c = do
              tbl :: Scoped.InfoTable <- (^. replContextArtifacts . artifactScopeTable) <$> replGetContext
              let def :: Scoped.ConstructorInfo = tbl ^?! Scoped.infoConstructors . at c . _Just
              return (def ^. Scoped.constructorInfoDef . Concrete.constructorDoc)

printDefinition :: String -> Repl ()
printDefinition = replParseIdentifiers >=> printIdentifiers
  where
    printIdentifiers :: NonEmpty Concrete.ScopedIden -> Repl ()
    printIdentifiers (d :| ds) = do
      printIdentifier d
      whenJust (nonEmpty ds) $ \ds' -> replNewline >> printIdentifiers ds'
      where
        getInfoTable :: Repl Scoped.InfoTable
        getInfoTable = (^. replContextArtifacts . artifactScopeTable) <$> replGetContext

        printIdentifier :: Concrete.ScopedIden -> Repl ()
        printIdentifier s = do
          case s of
            Concrete.ScopedAxiom a -> printAxiom (a ^. Concrete.axiomRefName . Scoped.nameId)
            Concrete.ScopedInductive a -> printInductive (a ^. Concrete.inductiveRefName . Scoped.nameId)
            Concrete.ScopedVar {} -> return ()
            Concrete.ScopedFunction f -> printFunction (f ^. Concrete.functionRefName . Scoped.nameId)
            Concrete.ScopedConstructor c -> printConstructor (c ^. Concrete.constructorRefName . Scoped.nameId)
          where
            printLocation :: HasLoc s => s -> Repl ()
            printLocation def = do
              s' <- ppConcrete s
              let txt :: Text = " is " <> prettyText (nameKindWithArticle (getNameKind s)) <> " defined at " <> prettyText (getLoc def)
              renderOutLn (s' <> mkAnsiText txt)

            printFunction :: Scoped.NameId -> Repl ()
            printFunction fun = do
              tbl :: Scoped.InfoTable <- getInfoTable
              let def :: Scoped.FunctionInfo = tbl ^?! Scoped.infoFunctions . at fun . _Just
              printLocation def
              printConcreteLn def

            printInductive :: Scoped.NameId -> Repl ()
            printInductive ind = do
              tbl :: Scoped.InfoTable <- (^. replContextArtifacts . artifactScopeTable) <$> replGetContext
              let def :: Concrete.InductiveDef 'Concrete.Scoped = tbl ^?! Scoped.infoInductives . at ind . _Just . Scoped.inductiveInfoDef
              printLocation def
              printConcreteLn def

            printAxiom :: Scoped.NameId -> Repl ()
            printAxiom ax = do
              tbl :: Scoped.InfoTable <- (^. replContextArtifacts . artifactScopeTable) <$> replGetContext
              let def :: Concrete.AxiomDef 'Concrete.Scoped = tbl ^?! Scoped.infoAxioms . at ax . _Just . Scoped.axiomInfoDef
              printLocation def
              printConcreteLn def

            printConstructor :: Scoped.NameId -> Repl ()
            printConstructor c = do
              tbl :: Scoped.InfoTable <- (^. replContextArtifacts . artifactScopeTable) <$> replGetContext
              let ind :: Scoped.Symbol = tbl ^?! Scoped.infoConstructors . at c . _Just . Scoped.constructorInfoTypeName
              printInductive (ind ^. Scoped.nameId)

inferType :: String -> Repl ()
inferType input = do
  gopts <- State.gets (^. replStateGlobalOptions)
  n <- replExpressionUpToTyped (strip (pack input))
  renderOutLn (Internal.ppOut (project' @GenericOptions gopts) (n ^. Internal.typedType))

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
          ("doc", printDocumentation),
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
    return $ case mmodulePath of
      Just path -> [i|#{unpack (P.prettyText path)}> |]
      Nothing -> "juvix> "

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

replExpressionUpToScopedAtoms :: Text -> Repl (Concrete.ExpressionAtoms 'Concrete.Scoped)
replExpressionUpToScopedAtoms txt = do
  ctx <- replGetContext
  x <-
    liftIO
      . runM
      . runError
      . evalState (ctx ^. replContextArtifacts)
      . runReader (ctx ^. replContextEntryPoint)
      $ expressionUpToAtomsScoped replPath txt
  replFromEither x

replExpressionUpToTyped :: Text -> Repl Internal.TypedExpression
replExpressionUpToTyped txt = do
  ctx <- replGetContext
  x <-
    liftIO
      . runM
      . runError
      . evalState (ctx ^. replContextArtifacts)
      . runReader (ctx ^. replContextEntryPoint)
      $ expressionUpToTyped replPath txt
  replFromEither x

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

replNewline :: Repl ()
replNewline = liftIO (putStrLn "")

renderOut :: (P.HasAnsiBackend a, P.HasTextBackend a) => a -> Repl ()
renderOut = render'

renderOutLn :: (P.HasAnsiBackend a, P.HasTextBackend a) => a -> Repl ()
renderOutLn t = renderOut t >> replNewline

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
