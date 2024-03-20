module Juvix.Data.Effect.Files
  ( module Juvix.Data.Effect.Files.Base,
    module Juvix.Data.Effect.Files.Pure,
    module Juvix.Data.Effect.Files.IO,
    module Juvix.Data.Effect.Files,
  )
where

import Data.HashSet qualified as HashSet
import Juvix.Data.Effect.Files.Base
import Juvix.Data.Effect.Files.IO
import Juvix.Data.Effect.Files.Pure (runFilesPure)
import Juvix.Extra.Paths.Base
import Juvix.Prelude.Base
import Juvix.Prelude.Path

-- | for now we only check for string equality
equalPaths :: Path Abs File -> Path Abs File -> Sem r Bool
equalPaths a b = return (a == b)

walkDirRelAccum ::
  forall acc r.
  (Member Files r) =>
  (Path Abs Dir -> [Path Rel Dir] -> [Path Rel File] -> acc -> Sem r (acc, Recurse Rel)) ->
  Path Abs Dir ->
  acc ->
  Sem r acc
walkDirRelAccum handler topdir' ini = execState ini (walkDirRel helper topdir')
  where
    helper :: Path Abs Dir -> [Path Rel Dir] -> [Path Rel File] -> Sem (State acc ': r) (Recurse Rel)
    helper cd dirs files = do
      (acc', r) <- get >>= raise . handler cd dirs files
      put acc'
      return r

walkDirRel ::
  forall r.
  (Member Files r) =>
  (Path Abs Dir -> [Path Rel Dir] -> [Path Rel File] -> Sem r (Recurse Rel)) ->
  Path Abs Dir ->
  Sem r ()
walkDirRel handler topdir = do
  let walkAvoidLoop :: Path Rel Dir -> Sem (State (HashSet Uid) ': r) ()
      walkAvoidLoop curdir =
        unlessM (checkLoop (topdir <//> curdir)) $
          walktree curdir
      walktree :: Path Rel Dir -> Sem (State (HashSet Uid) ': r) ()
      walktree curdir = do
        let fullDir :: Path Abs Dir = topdir <//> curdir
        (subdirs, files) <- listDirRel fullDir
        action <- raise (handler fullDir subdirs files)
        case action of
          RecurseNever -> return ()
          RecurseFilter fi -> do
            ds <- filterM filterSubdirs subdirs
            mapM_ (walkAvoidLoop . (curdir <//>)) ds
            where
              filterSubdirs :: Path Rel Dir -> Sem (State (HashSet Uid) ': r) Bool
              filterSubdirs d = do
                hasJuvixPackage <-
                  orM
                    ( fileExists' . (rootDir <//>) <$> [juvixYamlFile, packageFilePath]
                    )
                return (fi hasJuvixPackage d)
                where
                  rootDir :: Path Abs Dir
                  rootDir = topdir <//> curdir <//> d

      checkLoop :: Path Abs Dir -> Sem (State (HashSet Uid) ': r) Bool
      checkLoop dir = do
        visited <- get
        ufid <- pathUid dir
        if
            | HashSet.member ufid visited -> return True
            | otherwise -> modify' (HashSet.insert ufid) $> False
  evalState mempty (walkAvoidLoop $(mkRelDir "."))

-- | Find all files relative to a root directory
relFiles :: (Member Files r) => Path Abs Dir -> Sem r (HashSet (Path Rel File))
relFiles root = walkDirRelAccum handler root mempty
  where
    handler ::
      Path Abs Dir ->
      [Path Rel Dir] ->
      [Path Rel File] ->
      HashSet (Path Rel File) ->
      Sem r (HashSet (Path Rel File), Recurse Rel)
    handler cd _ fs acc = return (acc <> HashSet.fromList (mkRel cd <$> fs), RecurseFilter (\_ _ -> True))

    mkRel :: Path Abs Dir -> Path Rel File -> Path Rel File
    mkRel cd f = fromJust (stripProperPrefix root (cd <//> f))

-- | Restore the original contents of a file if an error occurs in an action.
restoreFileOnError :: forall r a. (Members '[EmbedIO, Files, TempFile] r) => Path Abs File -> Sem r a -> Sem r a
restoreFileOnError p action = do
  t <- tempFilePath
  finally (restoreOnErrorAction t) (removeTempFile t)
  where
    restoreOnErrorAction :: Path Abs File -> Sem r a
    restoreOnErrorAction tmpFile = do
      copyFile' p tmpFile
      onException action (renameFile' tmpFile p)

globalYaml :: (Members '[Files] r) => Sem r (Path Abs File)
globalYaml = (<//> juvixYamlFile) <$> globalRoot

globalPackageJuvix :: (Members '[Files] r) => Sem r (Path Abs File)
globalPackageJuvix = (<//> packageFilePath) <$> globalRoot

globalRoot :: (Members '[Files] r) => Sem r (Path Abs Dir)
globalRoot = (<//> $(mkRelDir "global-project")) <$> juvixConfigDir

globalPackageDescriptionRoot :: (Members '[Files] r) => Sem r (Path Abs Dir)
globalPackageDescriptionRoot = (<//> $(mkRelDir "package")) <$> juvixConfigDir

globalPackageBaseRoot :: (Members '[Files] r) => Sem r (Path Abs Dir)
globalPackageBaseRoot = (<//> $(mkRelDir "package-base")) <$> juvixConfigDir
