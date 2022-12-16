{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid restricted extensions" #-}
{-# HLINT ignore "Avoid restricted flags" #-}

module Juvix.Prelude.Path.OrphanInstances where

import Juvix.Prelude.Base
import Path
import Path.IO
import Prettyprinter

instance Pretty (Path a b) where
  pretty = pretty . toFilePath

deriving stock instance Data b => Data (SomeBase b)

instance AnyPath (SomeBase File) where
  type AbsPath (SomeBase File) = Path Abs File
  type RelPath (SomeBase File) = Path Rel File

  canonicalizePath :: MonadIO m => SomeBase File -> m (Path Abs File)
  canonicalizePath = \case
    Abs a -> canonicalizePath a
    Rel a -> canonicalizePath a

  makeAbsolute :: MonadIO m => SomeBase File -> m (Path Abs File)
  makeAbsolute = \case
    Abs a -> makeAbsolute a
    Rel a -> makeAbsolute a

  makeRelative :: MonadThrow m => Path Abs Dir -> SomeBase File -> m (Path Rel File)
  makeRelative r = \case
    Abs a -> makeRelative r a
    Rel a -> makeRelative r a

  makeRelativeToCurrentDir :: MonadIO m => SomeBase File -> m (Path Rel File)
  makeRelativeToCurrentDir = \case
    Abs a -> makeRelativeToCurrentDir a
    Rel a -> makeRelativeToCurrentDir a

instance AnyPath (SomeBase Dir) where
  type AbsPath (SomeBase Dir) = Path Abs Dir
  type RelPath (SomeBase Dir) = Path Rel Dir

  canonicalizePath :: MonadIO m => SomeBase Dir -> m (Path Abs Dir)
  canonicalizePath = \case
    Abs a -> canonicalizePath a
    Rel a -> canonicalizePath a

  makeAbsolute :: MonadIO m => SomeBase Dir -> m (Path Abs Dir)
  makeAbsolute = \case
    Abs a -> makeAbsolute a
    Rel a -> makeAbsolute a

  makeRelative :: MonadThrow m => Path Abs Dir -> SomeBase Dir -> m (Path Rel Dir)
  makeRelative r = \case
    Abs a -> makeRelative r a
    Rel a -> makeRelative r a

  makeRelativeToCurrentDir :: MonadIO m => SomeBase Dir -> m (Path Rel Dir)
  makeRelativeToCurrentDir = \case
    Abs a -> makeRelativeToCurrentDir a
    Rel a -> makeRelativeToCurrentDir a
