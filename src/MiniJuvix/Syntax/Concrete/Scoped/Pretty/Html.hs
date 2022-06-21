module MiniJuvix.Syntax.Concrete.Scoped.Pretty.Html (genHtml, Theme (..)) where

import Data.ByteString qualified as BS
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy (toStrict)
import Data.Time.Clock
import Data.Time.Format
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Language
import MiniJuvix.Syntax.Concrete.Scoped.Name qualified as S
import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Base
import MiniJuvix.Syntax.Concrete.Scoped.Utils
import MiniJuvix.Utils.Paths
import MiniJuvix.Utils.Version
import Prettyprinter
import Prettyprinter.Render.Util.SimpleDocTree
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text qualified as Html
import Text.Blaze.Html5 as Html hiding (map)
import Text.Blaze.Html5.Attributes qualified as Attr

data Theme
  = Nord
  | Ayu
  deriving stock (Show)

genHtml :: Options -> Bool -> Theme -> FilePath -> Bool -> Module 'Scoped 'ModuleTop -> IO ()
genHtml opts recursive theme outputDir printMetadata entry = do
  createDirectoryIfMissing True outputDir
  copyAssetFiles
  withCurrentDirectory outputDir $ do
    mapM_ outputModule allModules
  where
    allModules
      | recursive = toList (getAllModules entry)
      | otherwise = pure entry

    copyAssetFiles :: IO ()
    copyAssetFiles = do
      createDirectoryIfMissing True toAssetsDir
      mapM_ writeAsset assetFiles
      where
        assetFiles :: [(FilePath, BS.ByteString)]
        assetFiles = $(assetsDir)

        writeAsset :: (FilePath, BS.ByteString) -> IO ()
        writeAsset (filePath, fileContents) =
          BS.writeFile (toAssetsDir </> takeFileName filePath) fileContents
        toAssetsDir = outputDir </> "assets"

    outputModule :: Module 'Scoped 'ModuleTop -> IO ()
    outputModule m = do
      createDirectoryIfMissing True (takeDirectory htmlFile)
      putStrLn $ "Writing " <> pack htmlFile
      utc <- getCurrentTime
      Text.writeFile htmlFile (genModule opts printMetadata utc theme m)
      where
        htmlFile = topModulePathToDottedPath (m ^. modulePath . S.nameConcrete) <.> ".html"

genModule :: Options -> Bool -> UTCTime -> Theme -> Module 'Scoped 'ModuleTop -> Text
genModule opts printMetadata utc theme m =
  toStrict $
    Html.renderHtml $
      docTypeHtml ! Attr.xmlns "http://www.w3.org/1999/xhtml" $
        mhead
          <> mbody
          <> if printMetadata then infoFooter else mempty
  where
    themeCss :: Html
    themeCss = case theme of
      Ayu -> ayuCss
      Nord -> nordCss

    prettySrc :: Html
    prettySrc =
      (pre ! Attr.id "src-content") $
        renderTree $ treeForm $ docStream' opts m

    mheader :: Html
    mheader =
      Html.div ! Attr.id "package-header" $
        (Html.span ! Attr.class_ "caption" $ "")

    mhead :: Html
    mhead =
      metaUtf8
        <> themeCss
        <> highlightJs

    mbody :: Html
    mbody =
      mheader
        <> prettySrc

    infoFooter :: Html
    infoFooter =
      footer . pre $
        toHtml ("Powered by " :: Text)
          <> (a ! Attr.href "https://heliaxdev.github.io/minijuvix" $ toHtml ("MiniJuvix CLI " :: Text))
          <> (a ! Attr.href (textValue commitAddress) $ toHtml versionTag)
          <> br
          <> Html.span (toHtml $ ("Last modified on " :: String) <> formattedTime)
      where
        commitAddress :: Text
        commitAddress = "https://github.com/heliaxdev/minijuvix/commit/" <> shortHash

        formattedTime = formatTime defaultTimeLocale "%Y-%m-%d %-H:%M %Z" utc

docStream' :: Options -> Module 'Scoped 'ModuleTop -> SimpleDocStream Ann
docStream' opts m = layoutPretty defaultLayoutOptions (runPrettyCode opts m)

renderTree :: SimpleDocTree Ann -> Html
renderTree = go

go :: SimpleDocTree Ann -> Html
go sdt = case sdt of
  STEmpty -> mempty
  STChar c -> toHtml c
  STText _ t -> toHtml t
  STLine s -> "\n" <> toHtml (textSpaces s)
  STAnn ann content -> putTag ann (go content)
  STConcat l -> mconcatMap go l
  where
    textSpaces :: Int -> Text
    textSpaces n = Text.replicate n (Text.singleton ' ')

putTag :: Ann -> Html -> Html
putTag ann x = case ann of
  AnnKind k -> tagKind k x
  AnnLiteralInteger -> Html.span ! Attr.class_ "ju-number" $ x
  AnnLiteralString -> Html.span ! Attr.class_ "ju-string" $ x
  AnnKeyword -> Html.span ! Attr.class_ "ju-keyword" $ x
  AnnUnkindedSym -> Html.span ! Attr.class_ "ju-var" $ x
  AnnDelimiter -> Html.span ! Attr.class_ "ju-delimiter" $ x
  AnnDef tmp ni -> tagDef tmp ni
  AnnRef tmp ni -> tagRef tmp ni
  where
    tagDef :: TopModulePath -> S.NameId -> Html
    tagDef tmp nid =
      Html.span ! Attr.id (nameIdAttr nid) $
        tagRef tmp nid

    tagRef tmp ni =
      Html.span ! Attr.class_ "annot" $
        a ! Attr.href (nameIdAttrRef tmp ni) $
          x
    tagKind k =
      Html.span
        ! Attr.class_
          ( case k of
              S.KNameConstructor -> "ju-constructor"
              S.KNameInductive -> "ju-inductive"
              S.KNameFunction -> "ju-function"
              S.KNameLocal -> "ju-var"
              S.KNameAxiom -> "ju-axiom"
              S.KNameLocalModule -> "ju-var"
              S.KNameTopModule -> "ju-var"
          )

nameIdAttr :: S.NameId -> AttributeValue
nameIdAttr (S.NameId k) = fromString . show $ k

nameIdAttrRef :: TopModulePath -> S.NameId -> AttributeValue
nameIdAttrRef tp s =
  topModulePathToDottedPath tp <> ".html" <> preEscapedToValue '#' <> nameIdAttr s

cssLink :: AttributeValue -> Html
cssLink css =
  link ! Attr.href css
    ! Attr.rel "stylesheet"
    ! Attr.type_ "text/css"

ayuCss :: Html
ayuCss = cssLink "assets/source-ayu-light.css"

nordCss :: Html
nordCss = cssLink "assets/source-nord.css"

highlightJs :: Html
highlightJs =
  script ! Attr.src "assets/highlight.js"
    ! Attr.type_ "text/javascript"
    $ mempty

metaUtf8 :: Html
metaUtf8 = meta ! Attr.charset "UTF-8"
