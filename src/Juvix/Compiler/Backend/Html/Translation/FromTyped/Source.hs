module Juvix.Compiler.Backend.Html.Translation.FromTyped.Source where

import Data.ByteString qualified as BS
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy (toStrict)
import Data.Time.Clock
import Data.Time.Format
import Juvix.Compiler.Backend.Html.Data.Theme
import Juvix.Compiler.Backend.Html.Extra
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Extra
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Pretty.Base
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver
import Juvix.Compiler.Internal.Pretty qualified as Internal
import Juvix.Extra.Paths
import Juvix.Extra.Version
import Juvix.Prelude
import Prettyprinter.Render.Util.SimpleDocTree
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text qualified as Html
import Text.Blaze.Html5 as Html hiding (map)
import Text.Blaze.Html5.Attributes qualified as Attr

data PlainHtmlOptions = PlainHtmlOptions
  { _htmlOptionsKind :: HtmlKind,
    _htmlOptionsBaseUrl :: Text
  }

makeLenses ''PlainHtmlOptions

kindSuffix :: HtmlKind -> String
kindSuffix = \case
  HtmlDoc -> ""
  HtmlSrc -> "-src"
  HtmlOnly -> ""

data PlainHtmlArgs = PlainHtmlArgs
  { _plainHtmlArgsConcreteOpts :: Options,
    _plainHtmlArgsRecursive :: Bool,
    _plainHtmlArgsTheme :: Theme,
    _plainHtmlArgsOutputDir :: Path Abs Dir,
    _plainHtmlArgsBaseUrl :: Text,
    _plainHtmlArgsPrintMetaData :: Bool,
    _plainHtmlArgsEntryPoint :: Module 'Scoped 'ModuleTop
  }

data GenModuleArgs = GenModuleArgs
  { _genModuleArgsConcreteOpts :: Options,
    _genModuleArgsHtmlKind :: HtmlKind,
    _genModuleArgsPrintMetata :: Bool,
    _genModuleArgsBaseUrl :: Text,
    _genModuleArgsUTC :: UTCTime,
    _genModuleArgsTheme :: Theme,
    _genModuleArgsEntryPoint :: Module 'Scoped 'ModuleTop
  }

data GenModuleHtmlArgs = GenModuleHtmlArgs
  { _genModuleHtmlArgsConcreteOpts :: Options,
    _genModuleHtmlArgsHtmlKind :: HtmlKind,
    _genModuleHtmlArgsPrintMetadata :: Bool,
    _genModuleHtmlArgsBaseUrl :: Text,
    _genModuleHtmlArgsUTC :: UTCTime,
    _genModuleHtmlArgsTheme :: Theme,
    _genModuleHtmlArgsEntryPoint :: Module 'Scoped 'ModuleTop
  }

makeLenses ''GenModuleArgs
makeLenses ''PlainHtmlArgs
makeLenses ''GenModuleHtmlArgs

genModule :: GenModuleArgs -> Text
genModule GenModuleArgs {..} =
  toStrict
    . Html.renderHtml
    . genModuleHtml
    $ GenModuleHtmlArgs
      { _genModuleHtmlArgsConcreteOpts = _genModuleArgsConcreteOpts,
        _genModuleHtmlArgsHtmlKind = _genModuleArgsHtmlKind,
        _genModuleHtmlArgsPrintMetadata = _genModuleArgsPrintMetata,
        _genModuleHtmlArgsBaseUrl = _genModuleArgsBaseUrl,
        _genModuleHtmlArgsUTC = _genModuleArgsUTC,
        _genModuleHtmlArgsTheme = _genModuleArgsTheme,
        _genModuleHtmlArgsEntryPoint = _genModuleArgsEntryPoint
      }

genPlainHtml :: PlainHtmlArgs -> IO ()
genPlainHtml o = do
  let outputDir = o ^. plainHtmlArgsOutputDir
  ensureDir outputDir
  copyAssetFiles
  withCurrentDir outputDir $ do
    mapM_ outputModule allModules
  where
    opts = o ^. plainHtmlArgsConcreteOpts
    recursive = o ^. plainHtmlArgsRecursive
    theme = o ^. plainHtmlArgsTheme
    printMetadata = o ^. plainHtmlArgsPrintMetaData
    entry = o ^. plainHtmlArgsEntryPoint
    baseUrl = o ^. plainHtmlArgsBaseUrl

    allModules
      | recursive = toList (getAllModules entry)
      | otherwise = pure entry

    copyAssetFiles :: IO ()
    copyAssetFiles = do
      ensureDir toAssetsDir
      mapM_ writeAsset assetFiles
      where
        assetFiles :: [(Path Rel File, BS.ByteString)]
        assetFiles = assetsDir

        writeAsset :: (Path Rel File, BS.ByteString) -> IO ()
        writeAsset (filePath, fileContents) =
          BS.writeFile (toFilePath (toAssetsDir <//> filePath)) fileContents
        toAssetsDir = (o ^. plainHtmlArgsOutputDir) <//> $(mkRelDir "assets")

    outputModule :: Module 'Scoped 'ModuleTop -> IO ()
    outputModule m = do
      ensureDir (parent htmlFile)
      putStrLn $ "Writing " <> pack (toFilePath htmlFile)
      utc <- getCurrentTime
      Text.writeFile
        (toFilePath htmlFile)
        ( genModule
            GenModuleArgs
              { _genModuleArgsConcreteOpts = opts,
                _genModuleArgsHtmlKind = HtmlOnly,
                _genModuleArgsPrintMetata = printMetadata,
                _genModuleArgsBaseUrl = baseUrl,
                _genModuleArgsUTC = utc,
                _genModuleArgsTheme = theme,
                _genModuleArgsEntryPoint = m
              }
        )
      where
        htmlFile :: Path Rel File
        htmlFile = relFile (topModulePathToDottedPath (m ^. modulePath . S.nameConcrete) <.> ".html")

genModuleHtml :: GenModuleHtmlArgs -> Html
genModuleHtml o =
  docTypeHtml ! Attr.xmlns "http://www.w3.org/1999/xhtml" $
    mhead
      <> mbody
      <> if printMetadata then infoFooter else mempty
  where
    opts = o ^. genModuleHtmlArgsConcreteOpts
    htmlKind = o ^. genModuleHtmlArgsHtmlKind
    printMetadata = o ^. genModuleHtmlArgsPrintMetadata
    baseUrl = o ^. genModuleHtmlArgsBaseUrl
    utc = o ^. genModuleHtmlArgsUTC
    theme = o ^. genModuleHtmlArgsTheme
    m = o ^. genModuleHtmlArgsEntryPoint

    themeCss :: Html
    themeCss = case theme of
      Ayu -> ayuCss
      Nord -> nordCss

    htmlOpts :: PlainHtmlOptions
    htmlOpts =
      PlainHtmlOptions
        { _htmlOptionsKind = htmlKind,
          _htmlOptionsBaseUrl = baseUrl
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

docStream' :: PrettyCode a => Options -> a -> SimpleDocStream Ann
docStream' opts m = layoutPretty defaultLayoutOptions (runPrettyCode opts m)

renderTree :: Members '[Reader PlainHtmlOptions] r => SimpleDocTree Ann -> Sem r Html
renderTree = go

ppCodeHtml' :: PrettyCode a => PlainHtmlOptions -> Options -> a -> Html
ppCodeHtml' htmlOpts opts = run . runReader htmlOpts . renderTree . treeForm . docStream' opts

ppCodeHtml :: (Members '[Reader PlainHtmlOptions] r, PrettyCode a) => a -> Sem r Html
ppCodeHtml x = do
  o <- ask
  return (ppCodeHtml' o defaultOptions x)

ppCodeHtmlInternal :: (Members '[Reader PlainHtmlOptions] r, Internal.PrettyCode a) => a -> Sem r Html
ppCodeHtmlInternal x = do
  o <- ask
  return (ppCodeHtmlInternal' o Internal.defaultOptions x)
  where
    ppCodeHtmlInternal' :: Internal.PrettyCode a => PlainHtmlOptions -> Internal.Options -> a -> Html
    ppCodeHtmlInternal' htmlOpts opts = run . runReader htmlOpts . renderTree . treeForm . docStreamInternal' opts
    docStreamInternal' :: Internal.PrettyCode a => Internal.Options -> a -> SimpleDocStream Ann
    docStreamInternal' opts m = layoutPretty defaultLayoutOptions (Internal.runPrettyCode opts m)

go :: Members '[Reader PlainHtmlOptions] r => SimpleDocTree Ann -> Sem r Html
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

putTag :: forall r. Members '[Reader PlainHtmlOptions] r => Ann -> Html -> Sem r Html
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
  AnnCode -> return x
  AnnImportant -> return x
  where
    boldDefine :: Sem r (Html -> Html)
    boldDefine =
      asks (^. htmlOptionsKind) <&> \case
        HtmlDoc -> Html.span ! Attr.class_ "ju-define"
        HtmlSrc -> id
        HtmlOnly -> id

    tagDef :: TopModulePath -> S.NameId -> Sem r Html
    tagDef tmp nid = do
      ref' <- tagRef tmp nid
      return $ (Html.span ! Attr.id (nameIdAttr nid)) ref'

    tagRef :: TopModulePath -> S.NameId -> Sem r Html
    tagRef tmp ni = do
      pth <- nameIdAttrRef tmp (Just ni)
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

moduleDocRelativePath :: Members '[Reader PlainHtmlOptions] r => TopModulePath -> Sem r (Path Rel File)
moduleDocRelativePath m = do
  suff <- kindSuffix <$> asks (^. htmlOptionsKind)
  return (topModulePathToRelativePathDot ".html" suff m)

nameIdAttrRef :: Members '[Reader PlainHtmlOptions] r => TopModulePath -> Maybe S.NameId -> Sem r AttributeValue
nameIdAttrRef tp s = do
  pth <- toFilePath <$> moduleDocRelativePath tp
  return (fromString pth <> preEscapedToValue '#' <>? (nameIdAttr <$> s))
