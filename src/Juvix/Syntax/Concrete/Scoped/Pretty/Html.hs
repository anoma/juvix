module Juvix.Syntax.Concrete.Scoped.Pretty.Html where

import Data.ByteString qualified as BS
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy (toStrict)
import Data.Time.Clock
import Data.Time.Format
import Juvix.Prelude
import Juvix.Prelude.Html
import Juvix.Syntax.Concrete.Language
import Juvix.Syntax.Concrete.Scoped.Name qualified as S
import Juvix.Syntax.Concrete.Scoped.Pretty.Base
import Juvix.Syntax.Concrete.Scoped.Utils
import Juvix.Utils.Paths
import Juvix.Utils.Version
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

data HtmlKind
  = HtmlDoc
  | HtmlSrc

newtype HtmlOptions = HtmlOptions
  { _htmlOptionsKind :: HtmlKind
  }

makeLenses ''HtmlOptions

kindSuffix :: HtmlKind -> String
kindSuffix = \case
  HtmlDoc -> ""
  HtmlSrc -> "-src"

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

genModuleHtml :: Options -> Bool -> UTCTime -> Theme -> Module 'Scoped 'ModuleTop -> Html
genModuleHtml opts printMetadata utc theme m =
  docTypeHtml ! Attr.xmlns "http://www.w3.org/1999/xhtml" $
    mhead
      <> mbody
      <> if printMetadata then infoFooter else mempty
  where
    themeCss :: Html
    themeCss = case theme of
      Ayu -> ayuCss
      Nord -> nordCss

    htmlOpts :: HtmlOptions
    htmlOpts =
      HtmlOptions
        { _htmlOptionsKind = HtmlSrc
        }

    pp :: PrettyCode a => a -> Html
    pp = ppCodeHtml' htmlOpts opts

    prettySrc :: Html
    prettySrc =
      (pre ! Attr.id "src-content") $
        pp m

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
          <> (a ! Attr.href "https://anoma.github.io/juvix" $ toHtml ("Juvix CLI " :: Text))
          <> (a ! Attr.href (textValue commitAddress) $ toHtml versionTag)
          <> br
          <> Html.span (toHtml $ ("Last modified on " :: String) <> formattedTime)
      where
        commitAddress :: Text
        commitAddress = "https://github.com/anoma/juvix/commit/" <> shortHash

        formattedTime = formatTime defaultTimeLocale "%Y-%m-%d %-H:%M %Z" utc

genModule :: Options -> Bool -> UTCTime -> Theme -> Module 'Scoped 'ModuleTop -> Text
genModule opts printMetadata utc theme =
  toStrict
    . Html.renderHtml
    . genModuleHtml opts printMetadata utc theme

docStream' :: PrettyCode a => Options -> a -> SimpleDocStream Ann
docStream' opts m = layoutPretty defaultLayoutOptions (runPrettyCode opts m)

renderTree :: Members '[Reader HtmlOptions] r => SimpleDocTree Ann -> Sem r Html
renderTree = go

ppCodeHtml' :: PrettyCode a => HtmlOptions -> Options -> a -> Html
ppCodeHtml' htmlOpts opts = run . runReader htmlOpts . renderTree . treeForm . docStream' opts

ppCodeHtml :: (Members '[Reader HtmlOptions] r, PrettyCode a) => a -> Sem r Html
ppCodeHtml x = do
  o <- ask
  return (ppCodeHtml' o defaultOptions x)

go :: Members '[Reader HtmlOptions] r => SimpleDocTree Ann -> Sem r Html
go sdt = case sdt of
  STEmpty -> return mempty
  STChar c -> return (toHtml c)
  STText _ t -> return (toHtml t)
  STLine s -> return ("\n" <> toHtml (textSpaces s))
  STAnn ann content -> go content >>= putTag ann
  STConcat l -> mconcatMap go l
  where
    textSpaces :: Int -> Text
    textSpaces n = Text.replicate n (Text.singleton ' ')

putTag :: forall r. Members '[Reader HtmlOptions] r => Ann -> Html -> Sem r Html
putTag ann x = case ann of
  AnnKind k -> return (tagKind k x)
  AnnLiteralInteger -> return (Html.span ! Attr.class_ "ju-number" $ x)
  AnnLiteralString -> return (Html.span ! Attr.class_ "ju-string" $ x)
  AnnKeyword -> return (Html.span ! Attr.class_ "ju-keyword" $ x)
  AnnUnkindedSym -> return (Html.span ! Attr.class_ "ju-var" $ x)
  AnnComment -> return (Html.span ! Attr.class_ "ju-var" $ x) -- TODO add comment class
  AnnDelimiter -> return (Html.span ! Attr.class_ "ju-delimiter" $ x)
  AnnDef tmp ni -> boldDefine <*> tagDef tmp ni
  AnnRef tmp ni -> tagRef tmp ni
  where
    boldDefine :: Sem r (Html -> Html)
    boldDefine =
      asks (^. htmlOptionsKind) <&> \case
        HtmlDoc -> Html.span ! Attr.class_ "ju-define"
        HtmlSrc -> id

    tagDef :: TopModulePath -> S.NameId -> Sem r Html
    tagDef tmp nid = do
      ref' <- tagRef tmp nid
      return $ (Html.span ! Attr.id (nameIdAttr nid)) ref'

    tagRef :: TopModulePath -> S.NameId -> Sem r Html
    tagRef tmp ni = do
      pth <- nameIdAttrRef tmp ni
      return $
        Html.span ! Attr.class_ "annot" $
          a ! Attr.href pth $
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

moduleDocRelativePath :: Members '[Reader HtmlOptions] r => TopModulePath -> Sem r FilePath
moduleDocRelativePath m = do
  suff <- kindSuffix <$> asks (^. htmlOptionsKind)
  return (topModulePathToRelativeFilePath (Just "html") suff joinDot m)
  where
    joinDot :: FilePath -> FilePath -> FilePath
    joinDot l r = l <.> r

nameIdAttrRef :: Members '[Reader HtmlOptions] r => TopModulePath -> S.NameId -> Sem r AttributeValue
nameIdAttrRef tp s = do
  pth <- moduleDocRelativePath tp
  return (fromString pth <> preEscapedToValue '#' <> nameIdAttr s)
