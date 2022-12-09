module Juvix.Prelude.Path
  ( module Juvix.Prelude.Path,
    module Path,
    module Path.IO,
  )
where

import Juvix.Prelude.Base
import Juvix.Prelude.Path.OrphanInstances ()
import Path hiding ((<.>), (</>))
import Path qualified
import Path.IO hiding (listDirRel, walkDirRel)

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

isHiddenDirectory :: Path b Dir -> Bool
isHiddenDirectory p = case toFilePath (dirname p) of
  "./" -> False
  '.' : _ -> True
  _ -> False

parseRelFile' :: FilePath -> Path Rel File
parseRelFile' = fromJust . parseRelFile

someBaseToAbs :: Path Abs Dir -> SomeBase b -> Path Abs b
someBaseToAbs root = \case
  Rel r -> root <//> r
  Abs a -> a

removeExtension :: Path b File -> Maybe (Path b File)
removeExtension = fmap fst . splitExtension

removeExtension' :: Path b File -> Path b File
removeExtension' = fromJust . fmap fst . splitExtension

replaceExtension' :: String -> Path b File -> Path b File
replaceExtension' ext = fromJust . replaceExtension ext

parents :: Path Abs a -> [Path Abs Dir]
parents = go [] . parent
  where
    go :: [Path Abs Dir] -> Path Abs Dir -> [Path Abs Dir]
    go ac p
      | isRoot p = reverse (p : ac)
      | otherwise = go (p : ac) (parent p)

withTempDir' :: (MonadIO m, MonadMask m) => (Path Abs Dir -> m a) -> m a
withTempDir' = withTempDir $(mkRelDir ".") "tmp"
