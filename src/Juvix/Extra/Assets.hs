module Juvix.Extra.Assets
  ( module Juvix.Extra.Assets,
  )
where

import Data.ByteString qualified as BS
import Juvix.Extra.Paths
import Juvix.Prelude.Base
import Juvix.Prelude.Path

data AssetKind
  = Css
  | Js
  | Images

assetsDirByKind :: AssetKind -> [(Path Rel File, ByteString)]
assetsDirByKind k = map (first relFile) $
  case k of
    Css -> $(cssDirQ)
    Js -> $(jsDirQ)
    Images -> $(imagesDirQ)

absDirAssetsByKind :: Path Abs Dir -> AssetKind -> Path Abs Dir
absDirAssetsByKind baseDir k = baseDir <//> $(mkRelDir "assets") <//> dir
  where
    dir :: Path Rel Dir
    dir = case k of
      Css -> $(mkRelDir "css")
      Js -> $(mkRelDir "js")
      Images -> $(mkRelDir "images")

assetsWithAbsPathAndContent :: Path Abs Dir -> [(Path Abs File, ByteString)]
assetsWithAbsPathAndContent baseDir =
  [ (absPath, content)
    | kind <- [Css, Js, Images],
      (relPart, content) <- assetsDirByKind kind,
      let absPath = absDirAssetsByKind baseDir kind <//> relPart
  ]

writeAssets :: Path Abs Dir -> IO ()
writeAssets baseDir = do
  putStrLn $ "Copying assets files to " <> pack (toFilePath baseDir)
  mapM_ writeAssetFile (assetsWithAbsPathAndContent baseDir)
  where
    writeAssetFile :: (Path Abs File, ByteString) -> IO ()
    writeAssetFile (p, content) = do
      let dirFile = parent p
      createDirIfMissing True dirFile
      BS.writeFile (toFilePath p) content
