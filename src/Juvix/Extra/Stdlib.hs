module Juvix.Extra.Stdlib where

import Juvix.Compiler.Pipeline.Package.Dependency
import Juvix.Data.Effect.Files
import Juvix.Data.Effect.TaggedLock
import Juvix.Extra.Files
import Juvix.Extra.Paths
import Juvix.Prelude

stdlibFiles :: [(Path Rel File, ByteString)]
stdlibFiles = juvixFiles $(stdlibDir)

ensureStdlib :: (Members '[TaggedLock, Files] r) => Path Abs Dir -> Path Abs Dir -> [Dependency] -> Sem r ()
ensureStdlib rootDir buildDir deps =
  whenJustM (packageStdlib rootDir buildDir deps) $ \stdlibRoot ->
    runReader stdlibRoot updateStdlib

packageStdlib :: forall r. (Members '[Files] r) => Path Abs Dir -> Path Abs Dir -> [Dependency] -> Sem r (Maybe (Path Abs Dir))
packageStdlib rootDir buildDir = firstJustM isStdLib
  where
    isStdLib :: Dependency -> Sem r (Maybe (Path Abs Dir))
    isStdLib = \case
      DependencyPath dep -> do
        adir <- canonicalDir rootDir (dep ^. pathDependencyPath)
        normBuildDir <- normalizeDir buildDir
        -- traceShowM ("rootDir", rootDir, "depPath", adir, "buildDir", buildDir, "normbuildDir", normBuildDir, "stdlibbuilddir", stdLibBuildDir)
        let mstdlib :: Maybe (Path Rel Dir) = stripProperPrefix normBuildDir adir
        return $
          if
              | mstdlib == Just relStdlibDir -> Just stdLibBuildDir
              | otherwise -> Nothing
        where
          stdLibBuildDir :: Path Abs Dir
          stdLibBuildDir = juvixStdlibDir buildDir
      DependencyGit {} -> return Nothing

writeStdlib :: forall r. (Members '[Reader OutputRoot, Files] r) => Sem r ()
writeStdlib = writeFiles stdlibFiles

updateStdlib :: (Members '[TaggedLock, Reader OutputRoot, Files] r) => Sem r ()
updateStdlib = updateFiles writeStdlib
