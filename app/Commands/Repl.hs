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
import Control.Monad.Trans.Reader (mapReaderT)
import Data.String.Interpolate (i)
import HaskelineJB
import Juvix.Compiler.Backend
import Juvix.Compiler.Concrete.Data.Name (absTopModulePath)
import Juvix.Compiler.Concrete.Data.Scope (scopePath)
import Juvix.Compiler.Concrete.Data.Scope qualified as Scoped
import Juvix.Compiler.Concrete.Data.ScopedName qualified as Scoped
import Juvix.Compiler.Concrete.Language qualified as Concrete
import Juvix.Compiler.Concrete.Pretty qualified as Concrete
import Juvix.Compiler.Core qualified as Core
import Juvix.Compiler.Core.Extra.Value
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Info.NoDisplayInfo qualified as Info
import Juvix.Compiler.Core.Pretty qualified as Core
import Juvix.Compiler.Internal.Language qualified as Internal
import Juvix.Compiler.Internal.Pretty qualified as Internal
import Juvix.Compiler.Pipeline.Repl
import Juvix.Compiler.Store.Extra
import Juvix.Data.CodeAnn (Ann)
import Juvix.Data.Error.GenericError qualified as Error
import Juvix.Data.NameKind
import Juvix.Extra.Paths qualified as P
import Juvix.Extra.Stdlib
import Juvix.Extra.Version
import Juvix.Prelude.Pretty qualified as P
import System.Console.ANSI qualified as Ansi
import System.Console.Haskeline
import System.Console.Repline
import System.Console.Repline qualified as Repline

printHelpTxt :: ReplOptions -> Repl ()
printHelpTxt opts = do
  liftIO $ do
    putStrLn normalCmds
    let isDev = opts ^. replIsDev
    when isDev (putStrLn devCmds)
  where
    normalCmds :: Text
    normalCmds =
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

    devCmds :: Text
    devCmds =
      [__i|
  :dev        DEV CMD       Command reserved for debugging
  |]

replDefaultLoc :: Interval
replDefaultLoc = singletonInterval (mkInitialLoc P.replPath)

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

welcomeMsg :: (MonadIO m) => m ()
welcomeMsg = liftIO (putStrLn [i|Juvix REPL version #{fullVersionDoc}: https://juvix.org. Run :help for help|])

multilineCmd :: String
multilineCmd = "multiline"

quit :: String -> Repl ()
quit _ = liftIO (throwIO Interrupt)

loadEntryPoint :: EntryPoint -> Repl ()
loadEntryPoint ep = do
  artif <- runReplPipelineIO ep
  let newCtx =
        ReplContext
          { _replContextArtifacts = artif,
            _replContextEntryPoint = ep
          }
  State.modify (set replStateContext (Just newCtx))
  let epPath :: Maybe (Path Abs File) = ep ^. entryPointModulePath
  whenJust epPath $ \path -> liftIO (putStrLn [i|OK loaded: #{toFilePath @String path}|])

reloadFile :: String -> Repl ()
reloadFile _ = replGetContext >>= loadEntryPoint . (^. replContextEntryPoint)

pSomeFile :: String -> Prepath File
pSomeFile = mkPrepath

loadFile :: Prepath File -> Repl ()
loadFile f = do
  entryPoint <- getReplEntryPointFromPrepath f
  loadEntryPoint entryPoint

loadDefaultPrelude :: Repl ()
loadDefaultPrelude =
  whenJustM
    defaultPreludeEntryPoint
    loadEntryPoint

getReplEntryPoint :: (Root -> a -> GlobalOptions -> IO EntryPoint) -> a -> Repl EntryPoint
getReplEntryPoint f inputFile = do
  root <- Reader.asks (^. replRoot)
  gopts <- State.gets (^. replStateGlobalOptions)
  liftIO
    ( set entryPointMainFile Nothing
        . set entryPointTarget (Just TargetCore)
        . set entryPointPipeline (Just PipelineEval)
        . set entryPointSymbolPruningMode KeepAll
        <$> f root inputFile gopts
    )

getReplEntryPointFromPrepath :: Prepath File -> Repl EntryPoint
getReplEntryPointFromPrepath = getReplEntryPoint (\r x -> runM . runTaggedLockPermissive . entryPointFromGlobalOptionsPre r (Just x))

getReplEntryPointFromPath :: Path Abs File -> Repl EntryPoint
getReplEntryPointFromPath = getReplEntryPoint (\r a -> runM . runTaggedLockPermissive . entryPointFromGlobalOptions r (Just a))

displayVersion :: String -> Repl ()
displayVersion _ = liftIO (putStrLn fullVersionDoc)

replCommand :: ReplOptions -> String -> Repl ()
replCommand opts input_ = catchAll $ do
  ctx <- replGetContext
  let tab = Core.computeCombinedInfoTable $ ctx ^. replContextArtifacts . artifactCoreModule
  evalRes <- compileThenEval ctx input_
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
          gopts <- State.gets (^. replStateGlobalOptions)
          ep <- getReplEntryPointFromPrepath (mkPrepath (toFilePath P.replPath))
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
          liftIO (doEvalIO' (project gopts ^. Core.optFieldSize) artif' n') >>= replFromEither

        doEvalIO' :: Natural -> Artifacts -> Core.Node -> IO (Either JuvixError Core.Node)
        doEvalIO' fsize artif' n =
          mapLeft (JuvixError @Core.CoreError)
            <$> Core.doEvalIO (Just fsize) False replDefaultLoc (Core.computeCombinedInfoTable $ artif' ^. artifactCoreModule) n

        compileString :: Repl (Maybe Core.Node)
        compileString = do
          (artifacts, res) <- compileReplInputIO' ctx (strip (pack s))
          res' <- replFromEither res
          State.modify (over (replStateContext . _Just) (set replContextArtifacts artifacts))
          return res'

core :: String -> Repl ()
core input_ = do
  ctx <- replGetContext
  opts <- Reader.asks (^. replOptions)
  compileRes <- compileReplInputIO' ctx (strip (pack input_)) >>= replFromEither . snd
  whenJust compileRes (renderOutLn . Core.ppOut opts)

dev :: String -> Repl ()
dev input_ = do
  ctx <- replGetContext
  if
      | input_ == scoperStateCmd -> do
          renderOutLn (Concrete.ppTrace (ctx ^. replContextArtifacts . artifactScoperState))
      | otherwise ->
          renderOutLn
            ( "Unrecognized command "
                <> input_
                <> "\nAvailable commands: "
                <> unwords cmds
            )
  where
    cmds :: [String]
    cmds = [scoperStateCmd]
    scoperStateCmd :: String
    scoperStateCmd = "scoperState"

ppConcrete :: (Concrete.PrettyPrint a) => a -> Repl AnsiText
ppConcrete a = do
  gopts <- State.gets (^. replStateGlobalOptions)
  let popts :: GenericOptions = project' gopts
  return (Concrete.ppOut popts a)

printConcrete :: (Concrete.PrettyPrint a) => a -> Repl ()
printConcrete = ppConcrete >=> renderOut

printConcreteLn :: (Concrete.PrettyPrint a) => a -> Repl ()
printConcreteLn = ppConcrete >=> renderOutLn

replParseIdentifiers :: String -> Repl (NonEmpty Concrete.ScopedIden)
replParseIdentifiers input_ =
  replExpressionUpToScopedAtoms (strip (pack input_))
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

getScopedInfoTable :: Repl Scoped.InfoTable
getScopedInfoTable = do
  artifs <- (^. replContextArtifacts) <$> replGetContext
  let tab0 = artifs ^. artifactScopeTable
  return $ tab0 <> computeCombinedScopedInfoTable (artifs ^. artifactModuleTable)

printDocumentation :: String -> Repl ()
printDocumentation = replParseIdentifiers >=> printIdentifiers
  where
    printIdentifiers :: NonEmpty Concrete.ScopedIden -> Repl ()
    printIdentifiers (d :| ds) = do
      printIdentifier d
      whenJust (nonEmpty ds) $ \ds' -> replNewline >> printIdentifiers ds'
      where
        printIdentifier :: Concrete.ScopedIden -> Repl ()
        printIdentifier s = do
          let n = s ^. Concrete.scopedIdenFinal . Scoped.nameId
          mdoc <- case getNameKind s of
            KNameAxiom -> getDocAxiom n
            KNameInductive -> getDocInductive n
            KNameLocal -> return Nothing
            KNameFunction -> getDocFunction n
            KNameConstructor -> getDocConstructor n
            KNameLocalModule -> impossible
            KNameTopModule -> impossible
            KNameAlias -> impossible
            KNameFixity -> impossible
          printDoc mdoc
          where
            printDoc :: Maybe (Concrete.Judoc 'Concrete.Scoped) -> Repl ()
            printDoc = \case
              Nothing -> do
                let s' :: Doc Ann = pretty s
                    msg = "No documentation available for" <+> s'
                renderOutLn (toAnsiText True msg)
              Just ju -> printConcrete ju

            getDocFunction :: Scoped.NameId -> Repl (Maybe (Concrete.Judoc 'Concrete.Scoped))
            getDocFunction fun = do
              tbl :: Scoped.InfoTable <- getScopedInfoTable
              let def = tbl ^?! Scoped.infoFunctions . at fun . _Just
              return (def ^. Concrete.functionDefDoc)

            getDocInductive :: Scoped.NameId -> Repl (Maybe (Concrete.Judoc 'Concrete.Scoped))
            getDocInductive ind = do
              tbl :: Scoped.InfoTable <- getScopedInfoTable
              let def :: Concrete.InductiveDef 'Concrete.Scoped = tbl ^?! Scoped.infoInductives . at ind . _Just
              return (def ^. Concrete.inductiveDoc)

            getDocAxiom :: Scoped.NameId -> Repl (Maybe (Concrete.Judoc 'Concrete.Scoped))
            getDocAxiom ax = do
              tbl :: Scoped.InfoTable <- getScopedInfoTable
              let def :: Concrete.AxiomDef 'Concrete.Scoped = tbl ^?! Scoped.infoAxioms . at ax . _Just
              return (def ^. Concrete.axiomDoc)

            getDocConstructor :: Scoped.NameId -> Repl (Maybe (Concrete.Judoc 'Concrete.Scoped))
            getDocConstructor c = do
              tbl :: Scoped.InfoTable <- getScopedInfoTable
              let def = tbl ^?! Scoped.infoConstructors . at c . _Just
              return (def ^. Concrete.constructorDoc)

printDefinition :: String -> Repl ()
printDefinition = replParseIdentifiers >=> printIdentifiers
  where
    printIdentifiers :: NonEmpty Concrete.ScopedIden -> Repl ()
    printIdentifiers (d :| ds) = do
      printIdentifier d
      whenJust (nonEmpty ds) $ \ds' -> replNewline >> printIdentifiers ds'
      where
        printIdentifier :: Concrete.ScopedIden -> Repl ()
        printIdentifier s =
          let n = s ^. Concrete.scopedIdenFinal . Scoped.nameId
           in case getNameKind s of
                KNameAxiom -> printAxiom n
                KNameInductive -> printInductive n
                KNameLocal -> return ()
                KNameFunction -> printFunction n
                KNameConstructor -> printConstructor n
                KNameLocalModule -> impossible
                KNameTopModule -> impossible
                KNameFixity -> impossible
                KNameAlias -> impossible
          where
            printLocation :: (HasLoc c) => c -> Repl ()
            printLocation def = do
              s' <- ppConcrete s
              let txt :: Text = " is " <> prettyText (nameKindWithArticle (getNameKind s)) <> " defined at " <> prettyText (getLoc def)
              renderOutLn (s' <> mkAnsiText txt)

            printFunction :: Scoped.NameId -> Repl ()
            printFunction fun = do
              tbl :: Scoped.InfoTable <- getScopedInfoTable
              case tbl ^. Scoped.infoFunctions . at fun of
                Just def -> do
                  printLocation def
                  printConcreteLn def
                Nothing -> return ()

            printInductive :: Scoped.NameId -> Repl ()
            printInductive ind = do
              tbl :: Scoped.InfoTable <- getScopedInfoTable
              let def :: Concrete.InductiveDef 'Concrete.Scoped = tbl ^?! Scoped.infoInductives . at ind . _Just
              printLocation def
              printConcreteLn def

            printAxiom :: Scoped.NameId -> Repl ()
            printAxiom ax = do
              tbl :: Scoped.InfoTable <- getScopedInfoTable
              let def :: Concrete.AxiomDef 'Concrete.Scoped = tbl ^?! Scoped.infoAxioms . at ax . _Just
              printLocation def
              printConcreteLn def

            printConstructor :: Scoped.NameId -> Repl ()
            printConstructor c = do
              tbl :: Scoped.InfoTable <- getScopedInfoTable
              let ind = tbl ^?! Scoped.infoConstructors . at c . _Just . Concrete.constructorInductiveName
              printInductive (ind ^. Scoped.nameId)

inferType :: String -> Repl ()
inferType input_ = do
  gopts <- State.gets (^. replStateGlobalOptions)
  n <- replExpressionUpToTyped (strip (pack input_))
  renderOutLn (Internal.ppOut (project' @GenericOptions gopts) (n ^. Internal.typedType))

replCommands :: ReplOptions -> [(String, String -> Repl ())]
replCommands opts = catchable ++ nonCatchable
  where
    nonCatchable :: [(String, String -> Repl ())]
    nonCatchable =
      [ ("quit", quit)
      ]
    catchable :: [(String, String -> Repl ())]
    catchable =
      map
        (second (catchAll .))
        [ ("help", const (printHelpTxt opts)),
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
          ("core", core),
          ("dev", dev)
        ]

mapInputT_ :: (m () -> m ()) -> InputT m () -> InputT m ()
mapInputT_ f =
  mkInputT
    . mapReaderT
      ( mapReaderT
          ( mapReaderT
              (mapReaderT (mapReaderT f))
          )
      )
    . unInputT

catchAll :: Repl () -> Repl ()
catchAll = Repline.dontCrash . catchJuvixError
  where
    catchJuvixError :: Repl () -> Repl ()
    catchJuvixError = mkHaskelineT . mapInputT_ catchErrorS . unHaskelineT
      where
        printErrorS :: JuvixError -> ReplS ()
        printErrorS e = do
          opts <- State.gets (^. replStateGlobalOptions)
          hasAnsi <- liftIO (Ansi.hSupportsANSIColor stderr)
          liftIO
            . hPutStrLn stderr
            . run
            . runReader (project' @GenericOptions opts)
            $ Error.render (renderType opts hasAnsi) Nothing e
          where
            renderType :: GlobalOptions -> Bool -> Error.RenderType
            renderType opts hasAnsi
              | opts ^. globalVSCode = Error.RenderVSCode
              | opts ^. globalNoColors || not hasAnsi = Error.RenderText
              | otherwise = Error.RenderAnsi

        catchErrorS :: ReplS () -> ReplS ()
        catchErrorS = (`Except.catchError` printErrorS)

defaultMatcher :: [(String, CompletionFunc ReplS)]
defaultMatcher = [(":load", fileCompleter)]

optsCompleter :: WordCompleter ReplS
optsCompleter n = do
  opts <- Reader.asks (^. replOptions)
  let names = (":" <>) . fst <$> replCommands opts
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
  r <- State.gets (^. replStateRoot . rootRootDir)
  putStrLn (pack (toFilePath r))

runCommand :: (Members '[EmbedIO, App, TaggedLock] r) => ReplOptions -> Sem r ()
runCommand opts = do
  root <- askRoot
  pkg <- askPackage
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
              options = replCommands opts,
              banner = replBanner
            }
  globalOptions <- askGlobalOptions
  let env =
        ReplEnv
          { _replRoot = root,
            _replOptions = opts,
            _replPackage = pkg
          }
      iniState =
        ReplState
          { _replStateRoot = root,
            _replStateContext = Nothing,
            _replStateGlobalOptions = globalOptions
          }
  e <-
    liftIO
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
  root <- State.gets (^. replStateRoot)
  let buildRoot = root ^. rootRootDir
      buildDir = resolveAbsBuildDir buildRoot (root ^. rootBuildDir)
  pkg <- Reader.asks (^. replPackage)
  mstdlibPath <- runM (runFilesIO (packageStdlib buildRoot buildDir (pkg ^. packageDependencies)))
  case mstdlibPath of
    Just stdlibPath ->
      Just
        . set entryPointResolverRoot stdlibPath
        <$> getReplEntryPointFromPath (stdlibPath <//> P.preludePath)
    Nothing -> return Nothing

replMakeAbsolute :: SomeBase b -> Repl (Path Abs b)
replMakeAbsolute = \case
  Abs p -> return p
  Rel r -> do
    invokeDir <- State.gets (^. replStateRoot . rootInvokeDir)
    return (invokeDir <//> r)

replExpressionUpToScopedAtoms :: Text -> Repl (Concrete.ExpressionAtoms 'Concrete.Scoped)
replExpressionUpToScopedAtoms txt = do
  ctx <- replGetContext
  x <-
    runM
      . runError
      . evalState (ctx ^. replContextArtifacts)
      . runReader (ctx ^. replContextEntryPoint)
      $ expressionUpToAtomsScoped P.replPath txt
  replFromEither x

replExpressionUpToTyped :: Text -> Repl Internal.TypedExpression
replExpressionUpToTyped txt = do
  ctx <- replGetContext
  x <-
    runM
      . runError
      . evalState (ctx ^. replContextArtifacts)
      . runReader (ctx ^. replContextEntryPoint)
      $ expressionUpToTyped P.replPath txt
  replFromEither x

compileReplInputIO' :: (MonadIO m) => ReplContext -> Text -> m (Artifacts, (Either JuvixError (Maybe Core.Node)))
compileReplInputIO' ctx txt =
  runM
    . runState (ctx ^. replContextArtifacts)
    . runReader (ctx ^. replContextEntryPoint)
    $ do
      r <- compileReplInputIO P.replPath txt
      return (extractNode <$> r)
  where
    extractNode :: ReplPipelineResult -> Maybe Core.Node
    extractNode = \case
      ReplPipelineResultNode n -> Just n
      ReplPipelineResultImport {} -> Nothing
      ReplPipelineResultOpen {} -> Nothing

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
