module Juvix.Compiler.Backend.Html.Translation.FromTyped.Source where

import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy (toStrict)
import Data.Time.Clock
import Data.Time.Format
import Juvix.Compiler.Backend.Html.Data.Options
import Juvix.Compiler.Backend.Html.Extra
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Extra
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Pretty.Base
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver
import Juvix.Compiler.Internal.Pretty qualified as Internal
import Juvix.Extra.Assets (writeAssets)
import Juvix.Extra.Strings qualified as Str
import Juvix.Extra.Version
import Juvix.Prelude
import Prettyprinter.Render.Util.SimpleDocTree
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text qualified as Html
import Text.Blaze.Html5 as Html hiding (map)
import Text.Blaze.Html5.Attributes qualified as Attr

kindSuffix :: HtmlKind -> String
kindSuffix = \case
  HtmlDoc -> ""
  HtmlSrc -> "-src"
  HtmlOnly -> ""

data GenSourceHtmlArgs = GenSourceHtmlArgs
  { _genSourceHtmlArgsConcreteOpts :: Options,
    _genSourceHtmlArgsAssetsDir :: Text,
    _genSourceHtmlArgsHtmlKind :: HtmlKind,
    _genSourceHtmlArgsParamBase :: Text,
    _genSourceHtmlArgsUrlPrefix :: Text,
    _genSourceHtmlArgsEntryPoint :: Module 'Scoped 'ModuleTop,
    _genSourceHtmlArgsOutputDir :: Path Abs Dir,
    _genSourceHtmlArgsPrintMetaData :: Bool,
    _genSourceHtmlArgsNonRecursive :: Bool,
    _genSourceHtmlArgsTheme :: Theme
  }

makeLenses ''GenSourceHtmlArgs

data GenModuleHtmlArgs = GenModuleHtmlArgs
  { _genModuleHtmlArgsConcreteOpts :: Options,
    _genModuleHtmlArgsPrintMetadata :: Bool,
    _genModuleHtmlArgsUTC :: UTCTime,
    _genModuleHtmlArgsEntryPoint :: Module 'Scoped 'ModuleTop
  }

makeLenses ''GenModuleHtmlArgs

data GenModuleArgs = GenModuleArgs
  { _genModuleArgsConcreteOpts :: Options,
    _genModuleArgsPrintMetata :: Bool,
    _genModuleArgsUTC :: UTCTime,
    _genModuleArgsEntryPoint :: Module 'Scoped 'ModuleTop
  }

makeLenses ''GenModuleArgs

genSourceHtml :: GenSourceHtmlArgs -> IO ()
genSourceHtml o@GenSourceHtmlArgs {..} = do
  let outputDir = _genSourceHtmlArgsOutputDir
  ensureDir outputDir
  writeAssets outputDir
  withCurrentDir outputDir $ do
    mapM_ outputModule allModules
  where
    entry = o ^. genSourceHtmlArgsEntryPoint

    htmlOptions :: HtmlOptions
    htmlOptions =
      HtmlOptions
        { _htmlOptionsOutputDir = o ^. genSourceHtmlArgsOutputDir,
          _htmlOptionsUrlPrefix = o ^. genSourceHtmlArgsUrlPrefix,
          _htmlOptionsAssetsPrefix = o ^. genSourceHtmlArgsAssetsDir,
          _htmlOptionsKind = o ^. genSourceHtmlArgsHtmlKind,
          _htmlOptionsParamBase = o ^. genSourceHtmlArgsParamBase,
          _htmlOptionsTheme = o ^. genSourceHtmlArgsTheme
        }

    allModules
      | _genSourceHtmlArgsNonRecursive = pure entry
      | otherwise = toList topModules

    topModules :: HashMap NameId (Module 'Scoped 'ModuleTop)
    topModules = getAllModules entry

    outputModule :: Module 'Scoped 'ModuleTop -> IO ()
    outputModule m = do
      ensureDir (parent htmlFile)
      let absPath = (htmlOptions ^. htmlOptionsOutputDir) <//> htmlFile
      putStrLn $ "Writing " <> pack (toFilePath absPath)
      utc <- getCurrentTime
      Text.writeFile
        (toFilePath htmlFile)
        ( run . runReader htmlOptions $
            genModuleText
              GenModuleArgs
                { _genModuleArgsConcreteOpts = o ^. genSourceHtmlArgsConcreteOpts,
                  _genModuleArgsPrintMetata = o ^. genSourceHtmlArgsPrintMetaData,
                  _genModuleArgsUTC = utc,
                  _genModuleArgsEntryPoint = m
                }
        )
      where
        htmlFile :: Path Rel File
        htmlFile = relFile (topModulePathToDottedPath (m ^. modulePath . S.nameConcrete) <.> ".html")

genModuleText ::
  forall r.
  Members '[Reader HtmlOptions] r =>
  GenModuleArgs ->
  Sem r Text
genModuleText GenModuleArgs {..} = do
  outputHtml <-
    genModuleHtml $
      GenModuleHtmlArgs
        { _genModuleHtmlArgsConcreteOpts = _genModuleArgsConcreteOpts,
          _genModuleHtmlArgsPrintMetadata = _genModuleArgsPrintMetata,
          _genModuleHtmlArgsUTC = _genModuleArgsUTC,
          _genModuleHtmlArgsEntryPoint = _genModuleArgsEntryPoint
        }
  return . toStrict . Html.renderHtml $ outputHtml

genModuleHtml ::
  forall r.
  Members '[Reader HtmlOptions] r =>
  GenModuleHtmlArgs ->
  Sem r Html
genModuleHtml o = do
  outHtml <- mhead <> mbody
  return $
    docTypeHtml ! Attr.xmlns "http://www.w3.org/1999/xhtml" $
      outHtml
  where
    mhead :: Sem r Html
    mhead = do
      css <- themeCss
      js <- highlightJs
      return $
        metaUtf8
          <> css
          <> js

    mbody :: Sem r Html
    mbody =
      fold
        [ mheader,
          prettySrc,
          infoFooter
        ]

    prettySrc :: Sem r Html
    prettySrc = do
      pp <-
        ppCodeHtml
          (o ^. genModuleHtmlArgsConcreteOpts)
          (o ^. genModuleHtmlArgsEntryPoint)
      return $ (pre ! Attr.id "src-content") pp

    mheader :: Sem r Html
    mheader =
      return $
        Html.div ! Attr.id "package-header" $
          (Html.span ! Attr.class_ "caption" $ "")

    infoFooter :: Sem r Html
    infoFooter
      | not (o ^. genModuleHtmlArgsPrintMetadata) = return mempty
      | otherwise =
          return $
            footer . pre $
              toHtml ("Powered by " :: Text)
                <> juvixLink
                <> commitInfo
                <> br
                <> formattedTime
      where
        juvixLink :: Html
        juvixLink =
          a ! Attr.href Str.juvixDotOrg $
            toHtml ("Juvix CLI " :: Text)

        commitInfo :: Html
        commitInfo =
          a
            ! Attr.href
              (textValue ("https://github.com/anoma/juvix/commit/" <> shortHash))
            $ toHtml versionTag

        formattedTime :: Html
        formattedTime =
          Html.span . toHtml $
            "Last modified on "
              <> formatTime defaultTimeLocale "%Y-%m-%d %-H:%M %Z" (o ^. genModuleHtmlArgsUTC)

docStream' :: PrettyCode a => Options -> a -> SimpleDocStream Ann
docStream' opts m = layoutPretty defaultLayoutOptions (runPrettyCode opts m)

renderTree :: Members '[Reader HtmlOptions] r => SimpleDocTree Ann -> Sem r Html
renderTree = go

ppCodeHtml' :: PrettyCode a => HtmlOptions -> Options -> a -> Html
ppCodeHtml' htmlOpts opts = run . runReader htmlOpts . renderTree . treeForm . docStream' opts

ppCodeHtml ::
  (Members '[Reader HtmlOptions] r, PrettyCode a) =>
  Options ->
  a ->
  Sem r Html
ppCodeHtml opts x = do
  o <- ask
  return (ppCodeHtml' o opts x)

ppCodeHtmlInternal :: (Members '[Reader HtmlOptions] r, Internal.PrettyCode a) => a -> Sem r Html
ppCodeHtmlInternal x = do
  o <- ask
  return (ppCodeHtmlInternal' o Internal.defaultOptions x)
  where
    ppCodeHtmlInternal' :: Internal.PrettyCode a => HtmlOptions -> Internal.Options -> a -> Html
    ppCodeHtmlInternal' htmlOpts opts = run . runReader htmlOpts . renderTree . treeForm . docStreamInternal' opts
    docStreamInternal' :: Internal.PrettyCode a => Internal.Options -> a -> SimpleDocStream Ann
    docStreamInternal' opts m = layoutPretty defaultLayoutOptions (Internal.runPrettyCode opts m)

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

moduleDocRelativePath :: Members '[Reader HtmlOptions] r => TopModulePath -> Sem r (Path Rel File)
moduleDocRelativePath m = do
  suff <- kindSuffix <$> asks (^. htmlOptionsKind)
  return (topModulePathToRelativePathDot ".html" suff m)

nameIdAttrRef :: Members '[Reader HtmlOptions] r => TopModulePath -> Maybe S.NameId -> Sem r AttributeValue
nameIdAttrRef tp s = do
  pth <- toFilePath <$> moduleDocRelativePath tp
  prefixUrl <- unpack <$> asks (^. htmlOptionsUrlPrefix)
  return $
    fromString prefixUrl
      <> fromString pth
      <> preEscapedToValue '#'
      <>? (nameIdAttr <$> s)
