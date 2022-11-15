module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver
  ( PathResolver,
    resolvePath,
    addDependency,
    runPathResolver,
    runPathResolverPipe,
    topModulePathToRelativeFilePath,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Compiler.Concrete.Data.Name
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Prelude
import System.FilePath.Find as Find

data PathResolver m a where
  ResolvePath :: TopModulePath -> PathResolver r (Either FilesError FilePath)
  AddDependency :: Dependency -> PathResolver r ()

makeSem ''PathResolver

data Root = Root
  { _rootFilePath :: FilePath,
    _rootRelativeFiles :: HashSet FilePath
  }

data ResolverState = ResolverState
  { _stateDefaultRoot :: FilePath,
    _stateRoots :: HashMap FilePath Root,
    _stateMap :: HashMap FilePath (NonEmpty Root)
  }

makeLenses ''Root
makeLenses ''ResolverState

iniResolverState :: FilePath -> ResolverState
iniResolverState f =
  ResolverState
    { _stateDefaultRoot = f,
      _stateRoots = mempty,
      _stateMap = mempty
    }

-- | the argument should be an absolute path
mkRoot :: Members '[Files] r => FilePath -> Sem r Root
mkRoot _rootFilePath = do
  fs <- map (normalise . makeRelative _rootFilePath) <$> filesFind always (extension ==? ".juvix") _rootFilePath
  let _rootRelativeFiles = HashSet.fromList fs
  return Root {..}

hasDependency :: Members '[State ResolverState] r => Dependency -> Sem r Bool
hasDependency d = HashMap.member (d ^. dependencyPath) <$> gets (^. stateRoots)

addDependency' :: Members '[State ResolverState, Files] r => Dependency -> Sem r ()
addDependency' d = do
  let p :: FilePath = d ^. dependencyPath
  unlessM (hasDependency d) $ do
    r <- mkRoot p
    modify' (set (stateRoots . at p) (Just r))
    forM_ (r ^. rootRelativeFiles) $ \f ->
      modify' (over (stateMap . at f) (Just . maybe (pure r) (NonEmpty.cons r)))

topModulePathToRelativeFilePath :: String -> String -> (FilePath -> FilePath -> FilePath) -> TopModulePath -> FilePath
topModulePathToRelativeFilePath ext suffix joinpath mp = relFilePath
  where
    relDirPath :: FilePath
    relDirPath = foldr (joinpath . toPath) mempty (mp ^. modulePathDir)
    relFilePath :: FilePath
    relFilePath = addExt (relDirPath `joinpath'` toPath (mp ^. modulePathName) <> suffix)
    joinpath' :: FilePath -> FilePath -> FilePath
    joinpath' l r
      | null l = r
      | otherwise = joinpath l r
    addExt = (<.> ext)
    toPath :: Symbol -> FilePath
    toPath s = unpack (s ^. symbolText)

resolvePath' :: Members '[State ResolverState] r => TopModulePath -> Sem r (Either FilesError FilePath)
resolvePath' mp = do
  let rel = topModulePathToRelativeFilePath ".juvix" "" (</>) mp
  m <- gets (^. stateMap . at rel)
  case m of
    Just (r :| rs) -> case rs of
      [] -> return (Right (r ^. rootFilePath </> rel))
      _ -> return (Left (error "file conflict"))
    Nothing -> do
      r <- gets (^. stateDefaultRoot)
      return (Right (r </> rel))

re :: Members '[Files] r => Sem (PathResolver ': r) a -> Sem (State ResolverState ': r) a
re = reinterpret $ \case
  AddDependency d -> addDependency' d
  ResolvePath p -> resolvePath' p

runPathResolver :: Members '[Files] r => FilePath -> Sem (PathResolver ': r) a -> Sem r a
runPathResolver root = evalState (iniResolverState root) . re

runPathResolverPipe :: Members '[Files, Reader EntryPoint] r => Sem (PathResolver ': r) a -> Sem r a
runPathResolverPipe a = do
  r <- asks (^. entryPointRoot)
  runPathResolver r a

-- stdlibOrFile ::
--   forall r.
--   Members '[Embed IO, Error FilesError] r =>
--   FilePath ->
--   FilePath ->
--   Maybe StdlibState ->
--   Sem r FilePath
-- stdlibOrFile p rootPath m = case m of
--   Nothing -> return (rootPath </> p)
--   Just s
--     | HashSet.member (normalise p) (s ^. stdlibFilePaths) ->
--         ifM
--           isConflict
--           ( throw
--               FilesError
--                 { _filesErrorPath = pAbsPath,
--                   _filesErrorCause = StdlibConflict
--                 }
--           )
--           (return (s ^. stdlibRoot </> p))
--     | otherwise -> return pAbsPath
--     where
--       pAbsPath :: FilePath
--       pAbsPath = rootPath </> p

--       isConflict :: Sem r Bool
--       isConflict = do
--         cRootPath <- embed (canonicalizePath rootPath)
--         cStdlibPath <- embed (canonicalizePath (s ^. stdlibRoot))
--         andM [return (cRootPath /= cStdlibPath), embed (doesFileExist pAbsPath)]
