module Juvix.Extra.Assets
  ( module Juvix.Extra.Assets,
  )
where

import Juvix.Extra.Paths
import Juvix.Prelude.Base
import Juvix.Prelude.Path
import Data.ByteString qualified as BS

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
  [ (absDirAssetsByKind baseDir kind <//> relPart, content)
    | kind <- [Css, Js, Images]
    , (relPart, content) <- assetsDirByKind kind
  ]

writeAssets :: Path Abs Dir -> IO ()
writeAssets baseDir = do
  ensureDir baseDir
  mapM_ (\(path, content)->
    BS.writeFile (toFilePath path) content)
    (assetsWithAbsPathAndContent baseDir)
