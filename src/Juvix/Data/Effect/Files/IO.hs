module Juvix.Data.Effect.Files.IO
  ( module Juvix.Data.Effect.Files.IO,
    module Juvix.Data.Effect.Files.Base,
  )
where

import Data.ByteString qualified as ByteString
import Data.HashSet qualified as HashSet
import Juvix.Data.Effect.Files.Base
import Juvix.Extra.Stdlib qualified as Stdlib
import Juvix.Prelude.Base

-- | Try to resolve a filepath `p` to a path within standard library.
--
-- When `p` is not a member of the standard library or no stanard library is
-- registered, resolve `p` to an absolute path based at `rootPath` instead.
--
-- This function throws an error if `p` is a member of the standard library and
-- also present within `rootPath`.
stdlibOrFile ::
  forall r.
  Members '[Embed IO, Error FilesError] r =>
  FilePath ->
  FilePath ->
  Maybe StdlibState ->
  Sem r FilePath
stdlibOrFile p rootPath m = case m of
  Nothing -> return (rootPath </> p)
  Just s
    | HashSet.member (normalise p) (s ^. stdlibFilePaths) ->
        ifM
          isConflict
          ( throw
              FilesError
                { _filesErrorPath = pAbsPath,
                  _filesErrorCause = StdlibConflict
                }
          )
          (return (s ^. stdlibRoot </> p))
    | otherwise -> return pAbsPath
    where
      pAbsPath :: FilePath
      pAbsPath = rootPath </> p

      isConflict :: Sem r Bool
      isConflict = do
        cRootPath <- embed (canonicalizePath rootPath)
        cStdlibPath <- embed (canonicalizePath (s ^. stdlibRoot))
        andM [return (cRootPath /= cStdlibPath), embed (doesFileExist pAbsPath)]

runFilesIO' ::
  forall r a r'.
  (r' ~ Embed IO : State FilesState ': Error FilesError ': r) =>
  FilePath ->
  Sem (Files ': r) a ->
  Sem r' a
runFilesIO' rootPath = reinterpret3 helper
  where
    helper :: forall rInitial x. Files (Sem rInitial) x -> Sem r' x
    helper = \case
      ReadFile' f -> embed (readFile f)
      ReadFileBS' f -> embed (ByteString.readFile f)
      FileExists' f -> embed (doesFileExist f)
      EqualPaths' f h ->
        embed
          ( do
              f' <- canonicalizePath f
              h' <- canonicalizePath h
              return (Just (equalFilePath f' h'))
          )
      RegisterStdlib stdlibRootPath -> do
        absStdlibRootPath <- embed (makeAbsolute stdlibRootPath)
        fs <- embed (getFilesRecursive absStdlibRootPath)
        let paths = normalise . makeRelative absStdlibRootPath <$> fs
        modify
          ( set
              stdlibState
              ( Just
                  StdlibState
                    { _stdlibRoot = absStdlibRootPath,
                      _stdlibFilePaths = HashSet.fromList paths
                    }
              )
          )
      UpdateStdlib p -> runReader p Stdlib.updateStdlib
      CanonicalizePath' f -> embed (canonicalizePath f)
      GetAbsPath f -> do
        s <- gets (^. stdlibState)
        p <- stdlibOrFile f rootPath s
        embed (canonicalizePath p)

runFilesIO :: Member (Embed IO) r => FilePath -> Sem (Files ': r) a -> Sem (Error FilesError ': r) a
runFilesIO rootPath = evalState initState . subsume . runFilesIO' rootPath
