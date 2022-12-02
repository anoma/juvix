module Juvix.Data.Effect.Files
  ( module Juvix.Data.Effect.Files.Error,
    module Juvix.Data.Effect.Files.Base,
    module Juvix.Data.Effect.Files.Pure,
    module Juvix.Data.Effect.Files.IO,
    module Juvix.Data.Effect.Files,
  )
where

import Data.IntSet qualified as IntSet
import Juvix.Data.Effect.Files.Base
import Juvix.Data.Effect.Files.Error
import Juvix.Data.Effect.Files.IO
import Juvix.Data.Effect.Files.Pure (runFilesPure)
import Juvix.Prelude.Base
import Juvix.Prelude.Path

walkDirRelAccum :: forall acc b r.
  Member Files r =>
  (Path Rel Dir -> [Path Rel Dir] -> [Path Rel File] -> acc -> Sem r (acc, Recurse Rel)) ->
  Path b Dir ->
  acc ->
  Sem r acc
walkDirRelAccum handler topdir' ini = execState ini (walkDirRel helper topdir')
  where
  helper :: Path Rel Dir -> [Path Rel Dir] -> [Path Rel File] -> Sem (State acc ': r) (Recurse Rel)
  helper cd dirs files = do
    (acc', r) <- get >>= raise . handler cd dirs files
    put acc'
    return r

walkDirRel ::
  forall b r.
  Member Files r =>
  (Path Rel Dir -> [Path Rel Dir] -> [Path Rel File] -> Sem r (Recurse Rel)) ->
  Path b Dir ->
  Sem r ()
walkDirRel handler topdir' = do
  topdir <- getDirAbsPath topdir'
  let walkAvoidLoop :: Path Rel Dir -> Sem (State IntSet ': r) ()
      walkAvoidLoop curdir =
        unlessM (checkLoop (topdir <//> curdir)) $
          walktree curdir
      walktree :: Path Rel Dir -> Sem (State IntSet ': r) ()
      walktree curdir = do
        (subdirs, files) <- listDirRel (topdir <//> curdir)
        action <- raise (handler curdir subdirs files)
        case action of
          RecurseNever -> return ()
          RecurseFilter fi ->
            let ds = map (curdir <//>) (filter fi subdirs)
             in mapM_ walkAvoidLoop ds
      checkLoop :: Path Abs Dir -> Sem (State IntSet ': r) Bool
      checkLoop dir = do
        visited <- get
        ufid <- hashPath dir
        if
          | IntSet.member ufid visited -> return True
          | otherwise -> modify' (IntSet.insert ufid) $> False
  void (evalState mempty $ walkAvoidLoop $(mkRelDir "."))
