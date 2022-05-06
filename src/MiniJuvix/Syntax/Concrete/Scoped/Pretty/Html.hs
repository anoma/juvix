module MiniJuvix.Syntax.Concrete.Scoped.Pretty.Html (genHtml, Theme (..)) where

import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy (toStrict)
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Language
import MiniJuvix.Syntax.Concrete.Scoped.Name qualified as S
import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Base
import MiniJuvix.Syntax.Concrete.Scoped.Utils
import MiniJuvix.Utils.Paths
import Prettyprinter
import Prettyprinter.Render.Util.SimpleDocTree
import Text.Blaze.Html.Renderer.Text qualified as Html
import Text.Blaze.Html5 as Html hiding (map)
import Text.Blaze.Html5.Attributes qualified as Attr

data Theme
  = Nord
  | Ayu
  deriving stock (Show)

genHtml :: Options -> Bool -> Theme -> Module 'Scoped 'ModuleTop -> IO ()
genHtml opts recursive theme entry = do
  createDirectoryIfMissing True htmlPath
  copyAssetFiles
  withCurrentDirectory htmlPath $ do
    mapM_ outputModule allModules
  where
    allModules
      | recursive = toList $ getAllModules entry
      | otherwise = pure entry
    htmlPath :: FilePath
    htmlPath = "html"

    copyAssetFiles :: IO ()
    copyAssetFiles = do
      createDirectoryIfMissing True toAssetsDir
      mapM_ cpFile assetFiles
      where
        fromAssetsDir :: FilePath
        fromAssetsDir = $(assetsDir)
        toAssetsDir = htmlPath </> "assets"
        cpFile (fromDir, name, toDir) = copyFile (fromDir </> name) (toDir </> name)
        assetFiles :: [(FilePath, FilePath, FilePath)]
        assetFiles =
          [ (fromAssetsDir, name, toAssetsDir)
            | name <-
                [ "highlight.js",
                  "source-ayu-light.css",
                  "source-nord.css"
                ]
          ]

    outputModule :: Module 'Scoped 'ModuleTop -> IO ()
    outputModule m = do
      createDirectoryIfMissing True (takeDirectory htmlFile)
      putStrLn $ "Writing " <> pack htmlFile
      Text.writeFile htmlFile (genModule opts theme m)
      where
        htmlFile = topModulePathToDottedPath (m ^. modulePath . S.nameConcrete) <.> ".html"

genModule :: Options -> Theme -> Module 'Scoped 'ModuleTop -> Text
genModule opts theme m =
  toStrict $
    Html.renderHtml $
      docTypeHtml ! Attr.xmlns "http://www.w3.org/1999/xhtml" $
        mhead
          <> mbody
  where
    themeCss = case theme of
      Ayu -> ayuCss
      Nord -> nordCss
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
