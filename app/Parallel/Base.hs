module Parallel.Base
  ( module Parallel.Base,
  )
where

import Data.HashSet qualified as HashSet
import Effectful.Concurrent.STM as STM
import Juvix.Prelude

type ModuleSimpleId = Text

data Module = Module
  { _moduleId :: ModuleSimpleId,
    _moduleDeps :: [ModuleSimpleId],
    _moduleLoad :: Double
  }
  deriving stock (Eq, Show)

data CompiledProof = CompiledProof
  deriving stock (Show)

data CompilationState = CompilationState
  { _compilationState :: HashMap ModuleSimpleId CompiledProof,
    -- | Initially populated with `Dependencies._dependenciesTable`. It
    -- is used to keep track of which dependencies need to be compiled before
    -- this module is enqueued for compilation.
    _compilationPending :: HashMap ModuleSimpleId (HashSet ModuleSimpleId),
    _compilationStartedNum :: Natural,
    _compilationFinishedNum :: Natural,
    _compilationTotalNum :: Natural
  }

newtype ModulesIndex = ModulesIndex
  { _modulesIndex :: HashMap ModuleSimpleId Module
  }

data Dependencies = Dependencies
  { _dependenciesTable :: HashMap ModuleSimpleId (HashSet ModuleSimpleId),
    -- if x ∈ T[y], then y imports x
    _dependenciesTableReverse :: HashMap ModuleSimpleId (HashSet ModuleSimpleId)
  }
  deriving stock (Show)

data CompilationError = Unexpected
  deriving stock (Show)

newtype CompileQueue = CompileQueue
  { _compileQueue :: TBQueue ModuleSimpleId
  }

newtype Logs = Logs
  { _logQueue :: TQueue Text
  }

makeLenses ''Logs
makeLenses ''CompileQueue
makeLenses ''Module
makeLenses ''ModulesIndex
makeLenses ''Dependencies
makeLenses ''CompilationState

moduleDependencies :: Dependencies -> ModuleSimpleId -> HashSet ModuleSimpleId
moduleDependencies deps m = fromMaybe mempty (deps ^. dependenciesTable . at m)

getModule ::
  (Members '[Reader ModulesIndex] r) =>
  ModuleSimpleId ->
  Sem r Module
getModule uid = asks (^?! modulesIndex . at uid . _Just)

compilationStateFinished :: CompilationState -> Bool
compilationStateFinished CompilationState {..} = _compilationFinishedNum == _compilationTotalNum

addCompiledModule :: Dependencies -> Module -> CompilationState -> (CompilationState, (Bool, [ModuleSimpleId]))
addCompiledModule deps m st = run . runState st $ do
  let uid = m ^. moduleId
      revDeps :: [ModuleSimpleId] = deps ^. dependenciesTableReverse . at uid . _Just . to toList
  modify (set (compilationState . at uid) (Just CompiledProof))
  modify (over compilationFinishedNum succ)
  isLast <- compilationStateFinished <$> get
  fmap (isLast,) . execOutputList . forM_ revDeps $ \s -> do
    modify (over (compilationPending . at s . _Just) (HashSet.delete uid))
    -- if there are no more pending dependencies, we push it to the queue
    pend <- gets (^. compilationPending . at s)
    case pend of
      Just p
        | null p -> output s
      _ -> return ()

emptyDependencies :: Dependencies
emptyDependencies =
  Dependencies
    { _dependenciesTable = mempty,
      _dependenciesTableReverse = mempty
    }

mkModulesIndex :: [Module] -> ModulesIndex
mkModulesIndex ms = ModulesIndex (hashMap [(m ^. moduleId, m) | m <- ms])

-- | cycles are not checked
mkDependencies :: ModulesIndex -> Dependencies
mkDependencies mi =
  run
    . execState emptyDependencies
    . evalVisitEmpty goModule
    . mapM_ (goModule . (^. moduleId))
    $ toList byId
  where
    byId :: HashMap ModuleSimpleId Module
    byId = mi ^. modulesIndex

    goModule ::
      (Members '[State Dependencies, Visit ModuleSimpleId] r) =>
      ModuleSimpleId ->
      Sem r ()
    goModule uid = do
      let Module {..} = byId ^?! at uid . _Just
          deps = hashSet _moduleDeps
      modify (set (dependenciesTable . at _moduleId) (Just deps))
      forM_ _moduleDeps $ \dep -> do
        modify (over (dependenciesTableReverse . at dep) (Just . HashSet.insert _moduleId . fromMaybe mempty))
        visit dep

smallModuleList :: [Module]
smallModuleList =
  [ Module
      { _moduleId = "CoreTypes",
        _moduleDeps = [],
        _moduleLoad = 1.5
      },
    Module
      { _moduleId = "DataParser",
        _moduleDeps = ["CoreTypes"],
        _moduleLoad = 1.0
      },
    Module
      { _moduleId = "NetworkUtils",
        _moduleDeps = ["CoreTypes"],
        _moduleLoad = 0.5
      }
  ]

mediumModuleList :: [Module]
mediumModuleList =
  [ Module
      { _moduleId = "CoreTypes",
        _moduleDeps = [],
        _moduleLoad = 2.0
      },
    Module
      { _moduleId = "DataParser",
        _moduleDeps = ["CoreTypes"],
        _moduleLoad = 3.5
      },
    Module
      { _moduleId = "NetworkUtils",
        _moduleDeps = ["CoreTypes"],
        _moduleLoad = 1.0
      },
    Module
      { _moduleId = "APIClient",
        _moduleDeps = ["DataParser", "NetworkUtils"],
        _moduleLoad = 2.5
      },
    Module
      { _moduleId = "UserInterface",
        _moduleDeps = ["NetworkUtils"],
        _moduleLoad = 2.2
      },
    Module
      { _moduleId = "IntegrationTests",
        _moduleDeps = ["APIClient", "UserInterface"],
        _moduleLoad = 1.0
      }
  ]

bigModuleList :: [Module]
bigModuleList =
  [ Module
      { _moduleId = "CoreTypes",
        _moduleDeps = [],
        _moduleLoad = 2.0
      },
    Module
      { _moduleId = "DataParser",
        _moduleDeps = ["CoreTypes"],
        _moduleLoad = 3.5
      },
    Module
      { _moduleId = "NetworkUtils",
        _moduleDeps = ["CoreTypes"],
        _moduleLoad = 1.0
      },
    Module
      { _moduleId = "APIClient",
        _moduleDeps = ["DataParser", "NetworkUtils"],
        _moduleLoad = 2.5
      },
    Module
      { _moduleId = "UserInterface",
        _moduleDeps = ["NetworkUtils"],
        _moduleLoad = 2.2
      },
    Module
      { _moduleId = "IntegrationTests",
        _moduleDeps = ["APIClient", "UserInterface"],
        _moduleLoad = 1.0
      },
    Module
      { _moduleId = "DatabaseLayer",
        _moduleDeps = ["CoreTypes"],
        _moduleLoad = 3.2
      },
    Module
      { _moduleId = "AuthenticationModule",
        _moduleDeps = ["DatabaseLayer", "NetworkUtils"],
        _moduleLoad = 2.8
      },
    Module
      { _moduleId = "LoggingFramework",
        _moduleDeps = [],
        _moduleLoad = 0.5
      },
    Module
      { _moduleId = "ConfigurationManager",
        _moduleDeps = [],
        _moduleLoad = 1.5
      },
    Module
      { _moduleId = "EmailService",
        _moduleDeps = ["NetworkUtils", "LoggingFramework"],
        _moduleLoad = 2.7
      },
    Module
      { _moduleId = "PaymentGateway",
        _moduleDeps = ["NetworkUtils", "DatabaseLayer", "LoggingFramework"],
        _moduleLoad = 3.0
      },
    Module
      { _moduleId = "SessionManager",
        _moduleDeps = ["AuthenticationModule", "DatabaseLayer"],
        _moduleLoad = 3.5
      },
    Module
      { _moduleId = "UserManagement",
        _moduleDeps = ["AuthenticationModule", "EmailService", "DatabaseLayer"],
        _moduleLoad = 3.9
      },
    Module
      { _moduleId = "ReportGenerator",
        _moduleDeps = ["DataParser", "DatabaseLayer"],
        _moduleLoad = 4.0
      },
    Module
      { _moduleId = "AnalyticsEngine",
        _moduleDeps = ["DataParser", "SessionManager"],
        _moduleLoad = 2.1
      },
    Module
      { _moduleId = "NotificationService",
        _moduleDeps = ["EmailService", "LoggingFramework"],
        _moduleLoad = 1.8
      },
    Module
      { _moduleId = "FileHandling",
        _moduleDeps = ["CoreTypes"],
        _moduleLoad = 1.2
      },
    Module
      { _moduleId = "CacheManager",
        _moduleDeps = ["CoreTypes"],
        _moduleLoad = 1.9
      },
    Module
      { _moduleId = "APIGateway",
        _moduleDeps = ["APIClient", "AuthenticationModule", "SessionManager"],
        _moduleLoad = 2.0
      }
  ]
