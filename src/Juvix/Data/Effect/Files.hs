module Juvix.Data.Effect.Files
  ( module Juvix.Data.Effect.Files.Error,
    module Juvix.Data.Effect.Files.Base,
    module Juvix.Data.Effect.Files.Pure,
    module Juvix.Data.Effect.Files.IO,
    module Juvix.Data.Effect.Files,
  )
where

import Data.HashSet qualified as HashSet
import Juvix.Data.Effect.Files.Base
import Juvix.Data.Effect.Files.Error
import Juvix.Data.Effect.Files.IO
import Juvix.Data.Effect.Files.Pure (runFilesPure)
import Juvix.Prelude.Base
import Juvix.Prelude.Path

walkDirRelAccum ::
  forall acc b r.
  Member Files r =>
  (Path b Dir -> [Path Rel Dir] -> [Path Rel File] -> acc -> Sem r (acc, Recurse Rel)) ->
  Path b Dir ->
  acc ->
  Sem r acc
walkDirRelAccum handler topdir' ini = execState ini (walkDirRel helper topdir')
  where
    helper :: Path b Dir -> [Path Rel Dir] -> [Path Rel File] -> Sem (State acc ': r) (Recurse Rel)
    helper cd dirs files = do
      (acc', r) <- get >>= raise . handler cd dirs files
      put acc'
      return r

walkDirRel ::
  forall b r.
  Member Files r =>
  (Path b Dir -> [Path Rel Dir] -> [Path Rel File] -> Sem r (Recurse Rel)) ->
  Path b Dir ->
  Sem r ()
walkDirRel handler topdir' = do
  topdir :: Path Abs Dir <- getDirAbsPath topdir'
  let walkAvoidLoop :: Path Rel Dir -> Sem (State (HashSet Uid) ': r) ()
      walkAvoidLoop curdir =
        unlessM (checkLoop (topdir <//> curdir)) $
          walktree curdir
      walktree :: Path Rel Dir -> Sem (State (HashSet Uid) ': r) ()
      walktree curdir = do
        let fullDir :: Path b Dir = topdir' <//> curdir
        (subdirs, files) <- listDirRel fullDir
        action <- raise (handler fullDir subdirs files)
        case action of
          RecurseNever -> return ()
          RecurseFilter fi ->
            let ds = map (curdir <//>) (filter fi subdirs)
             in mapM_ walkAvoidLoop ds
      checkLoop :: Path Abs Dir -> Sem (State (HashSet Uid) ': r) Bool
      checkLoop dir = do
        visited <- get
        ufid <- pathUid dir
        if
            | HashSet.member ufid visited -> return True
            | otherwise -> modify' (HashSet.insert ufid) $> False
  evalState mempty (walkAvoidLoop $(mkRelDir "."))
