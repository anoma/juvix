module Juvix.Prelude.Path
  ( module Juvix.Prelude.Path,
    module Path,
    module Path.IO,
  )
where

import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Prelude.Base
import Juvix.Prelude.Path.OrphanInstances ()
import Path hiding ((<.>), (</>))
import Path qualified
import Path.IO hiding (listDirRel, walkDirRel)
import Path.Internal

absDir :: FilePath -> Path Abs Dir
absDir r = fromMaybe (error ("not an absolute file path: " <> pack r)) (parseAbsDir r)

infixr 5 <//>

-- | Synonym for Path.</>. Useful to avoid name clashes
(<//>) :: Path b Dir -> Path Rel t -> Path b t
(<//>) = (Path.</>)

someFile :: FilePath -> SomeBase File
someFile r = fromMaybe (error ("not a file path: " <> pack r)) (parseSomeFile r)

someDir :: FilePath -> SomeBase Dir
someDir r = fromMaybe (error ("not a dir path: " <> pack r)) (parseSomeDir r)

relFile :: FilePath -> Path Rel File
relFile r = fromMaybe (error ("not a relative file path: " <> pack r)) (parseRelFile r)

relDir :: FilePath -> Path Rel Dir
relDir r = fromMaybe (error ("not a relative directory path: " <> pack r)) (parseRelDir r)

absFile :: FilePath -> Path Abs File
absFile r = fromMaybe (error ("not an absolute file path: " <> pack r)) (parseAbsFile r)

destructAbsDir :: Path Abs Dir -> (Path Abs Dir, [Path Rel Dir])
destructAbsDir d = go d []
  where
    go :: Path Abs Dir -> [Path Rel Dir] -> (Path Abs Dir, [Path Rel Dir])
    go p acc
      | isRoot p = (p, acc)
      | otherwise = go (parent p) (dirname p : acc)

isRoot :: Path a Dir -> Bool
isRoot p = parent p == p

-- | is the root of absolute files always "/" ?
destructAbsFile :: Path Abs File -> (Path Abs Dir, [Path Rel Dir], Path Rel File)
destructAbsFile x = (root, dirs, filename x)
  where
    (root, dirs) = destructAbsDir (parent x)

isJuvixFile :: Path b File -> Bool
isJuvixFile = (== Just ".juvix") . fileExtension

isJuvixGebFile :: Path b File -> Bool
isJuvixGebFile = (== Just ".geb") . fileExtension

isLispFile :: Path b File -> Bool
isLispFile = (== Just ".lisp") . fileExtension

isHiddenDirectory :: Path b Dir -> Bool
isHiddenDirectory p
  | toFilePath p == relRootFP = False
  | otherwise = case toFilePath (dirname p) of
      '.' : _ -> True
      _ -> False

someBaseToAbs :: Path Abs Dir -> SomeBase b -> Path Abs b
someBaseToAbs root = \case
  Rel r -> root <//> r
  Abs a -> a

removeExtensions :: Path b File -> Path b File
removeExtensions p = maybe p removeExtensions (removeExtension p)

removeExtension :: Path b File -> Maybe (Path b File)
removeExtension = fmap fst . splitExtension

removeExtension' :: Path b File -> Path b File
removeExtension' = fst . fromJust . splitExtension

addExtensions :: (MonadThrow m) => [String] -> Path b File -> m (Path b File)
addExtensions ext p = case ext of
  [] -> return p
  (e : es) -> addExtension e p >>= addExtensions es

replaceExtensions :: (MonadThrow m) => [String] -> Path b File -> m (Path b File)
replaceExtensions ext = addExtensions ext . removeExtensions

replaceExtensions' :: [String] -> Path b File -> Path b File
replaceExtensions' ext = fromJust . replaceExtensions ext

addExtensions' :: [String] -> Path b File -> Path b File
addExtensions' ext = fromJust . addExtensions ext

addExtension' :: String -> Path b File -> Path b File
addExtension' ext = fromJust . addExtension ext

replaceExtension' :: String -> Path b File -> Path b File
replaceExtension' ext = fromJust . replaceExtension ext

dirnameToFile :: Path x Dir -> Path Rel File
dirnameToFile = relFile . dropTrailingPathSeparator . toFilePath . dirname

parents :: Path Abs a -> NonEmpty (Path Abs Dir)
parents = go [] . parent
  where
    go :: [Path Abs Dir] -> Path Abs Dir -> NonEmpty (Path Abs Dir)
    go ac p
      | isRoot p = NonEmpty.reverse (p :| ac)
      | otherwise = go (p : ac) (parent p)

withTempDir' :: (MonadIO m, MonadMask m) => (Path Abs Dir -> m a) -> m a
withTempDir' = withSystemTempDir "tmp"
