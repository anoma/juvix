{-# OPTIONS_GHC -Wno-orphans #-}

module Juvix.Prelude.Path
  ( module Juvix.Prelude.Path,
    module Path,
    module Path.IO,
  )
where

import Juvix.Prelude.Base
import Path hiding ((<.>), (</>))
import Path qualified
import Path.IO hiding (doesFileExist, listDirRel, walkDirRel)
import Prettyprinter

-- | Synonym for Path.</>. Useful to avoid name clashes
infixr 5 <//>

-- | this orphan instance is very convenient
instance Pretty (Path a b) where
  pretty = pretty . toFilePath

(<//>) :: Path b Dir -> Path Rel t -> Path b t
(<//>) = (Path.</>)

relFile :: FilePath -> Path Rel File
relFile r = fromMaybe (error ("not a relative file path: " <> pack r)) (parseRelFile r)

relDir :: FilePath -> Path Rel Dir
relDir r = fromMaybe (error ("not a relative directory path: " <> pack r)) (parseRelDir r)

absFile :: FilePath -> Path Abs File
absFile r = fromMaybe (error ("not an absolute file path: " <> pack r)) (parseAbsFile r)

absDir :: FilePath -> Path Abs Dir
absDir r = fromMaybe (error ("not an absolute file path: " <> pack r)) (parseAbsDir r)

destructAbsDir :: Path Abs Dir -> (Path Abs Dir, [Path Rel Dir])
destructAbsDir d = go d []
  where
    go :: Path Abs Dir -> [Path Rel Dir] -> (Path Abs Dir, [Path Rel Dir])
    go p acc
      | isRoot p = (p, acc)
      | otherwise = go (parent p) (dirname p : acc)
    isRoot :: Path Abs Dir -> Bool
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
