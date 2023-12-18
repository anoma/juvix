module Juvix.Compiler.Backend.Html.Translation.FromTyped.Source where

import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy (toStrict)
import Data.Time.Clock
import Data.Time.Format
import Juvix.Compiler.Backend.Html.Data.Options
import Juvix.Compiler.Backend.Html.Extra
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Print
import Juvix.Compiler.Internal.Pretty qualified as Internal
import Juvix.Compiler.Pipeline.Loader.PathResolver
import Juvix.Extra.Assets (writeAssets)
import Juvix.Prelude
import Prettyprinter
import Prettyprinter.Render.Util.SimpleDocTree
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text qualified as Html
import Text.Blaze.Html5 as Html hiding (map)
import Text.Blaze.Html5.Attributes qualified as Attr

data CssColor
  = JuInductive
  | JuConstructor
  | JuFunction
  | JuAxiom
  | JuString
  | JuKeyword
  | JuDelimiter
  | JuVar
  | JuFixity
  | JuNumber
  | JuComment
  | JuJudoc

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
    _genSourceHtmlArgsIdPrefix :: Text,
    _genSourceHtmlArgsOnlyCode :: Bool,
    _genSourceHtmlArgsModule :: Module 'Scoped 'ModuleTop,
    _genSourceHtmlArgsOutputDir :: Path Abs Dir,
    _genSourceHtmlArgsNonRecursive :: Bool,
    _genSourceHtmlArgsNoFooter :: Bool,
    _genSourceHtmlArgsNoPath :: Bool,
    _genSourceHtmlArgsComments :: Comments,
    _genSourceHtmlArgsTheme :: Theme
  }

makeLenses ''GenSourceHtmlArgs

data GenModuleHtmlArgs = GenModuleHtmlArgs
  { _genModuleHtmlArgsConcreteOpts :: Options,
    _genModuleHtmlArgsUTC :: UTCTime,
    _genModuleHtmlArgsComments :: Comments,
    _genModuleHtmlArgsModule :: Module 'Scoped 'ModuleTop
  }

makeLenses ''GenModuleHtmlArgs

data GenModuleTextArgs = GenModuleTextArgs
  { _genModuleTextArgsConcreteOpts :: Options,
    _genModuleTextArgsUTC :: UTCTime,
    _genModuleTextArgsComments :: Comments,
    _genModuleTextArgsModule :: Module 'Scoped 'ModuleTop
  }

makeLenses ''GenModuleTextArgs

genSourceHtml :: GenSourceHtmlArgs -> IO ()
genSourceHtml o@GenSourceHtmlArgs {..} = do
  let outputDir = _genSourceHtmlArgsOutputDir
  ensureDir outputDir
  writeAssets outputDir
  withCurrentDir outputDir $ do
    mapM_ outputModule allModules
  where
    htmlOptions :: HtmlOptions
    htmlOptions =
      HtmlOptions
        { _htmlOptionsOutputDir = o ^. genSourceHtmlArgsOutputDir,
          _htmlOptionsUrlPrefix = o ^. genSourceHtmlArgsUrlPrefix,
          _htmlOptionsIdPrefix = o ^. genSourceHtmlArgsIdPrefix,
          _htmlOptionsOnlyCode = o ^. genSourceHtmlArgsOnlyCode,
          _htmlOptionsAssetsPrefix = o ^. genSourceHtmlArgsAssetsDir,
          _htmlOptionsKind = o ^. genSourceHtmlArgsHtmlKind,
          _htmlOptionsParamBase = o ^. genSourceHtmlArgsParamBase,
          _htmlOptionsTheme = o ^. genSourceHtmlArgsTheme,
          _htmlOptionsNoFooter = o ^. genSourceHtmlArgsNoFooter,
          _htmlOptionsNoPath = o ^. genSourceHtmlArgsNoPath
        }

    entry = o ^. genSourceHtmlArgsModule

    allModules
      | _genSourceHtmlArgsNonRecursive = pure entry
      | otherwise = toList topModules

    -- TODO: top modules
    topModules :: HashMap NameId (Module 'Scoped 'ModuleTop)
    topModules = HashMap.fromList [(entry ^. modulePath . S.nameId, entry)]

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
              GenModuleTextArgs
                { _genModuleTextArgsConcreteOpts = o ^. genSourceHtmlArgsConcreteOpts,
                  _genModuleTextArgsUTC = utc,
                  _genModuleTextArgsComments = _genSourceHtmlArgsComments,
                  _genModuleTextArgsModule = m
                }
        )
      where
        htmlFile :: Path Rel File
        htmlFile = relFile (topModulePathToDottedPath (m ^. modulePath . S.nameConcrete) <.> ".html")

genModuleText ::
  forall r.
  (Members '[Reader HtmlOptions] r) =>
  GenModuleTextArgs ->
  Sem r Text
genModuleText GenModuleTextArgs {..} = do
  outputHtml <-
    genModuleHtml $
      GenModuleHtmlArgs
        { _genModuleHtmlArgsConcreteOpts = _genModuleTextArgsConcreteOpts,
          _genModuleHtmlArgsUTC = _genModuleTextArgsUTC,
          _genModuleHtmlArgsComments = _genModuleTextArgsComments,
          _genModuleHtmlArgsModule = _genModuleTextArgsModule
        }
  return . toStrict . Html.renderHtml $ outputHtml

genModuleHtml ::
  forall r.
  (Members '[Reader HtmlOptions] r) =>
  GenModuleHtmlArgs ->
  Sem r Html
genModuleHtml o = do
  onlyCode <- asks (^. htmlOptionsOnlyCode)
  if
      | onlyCode -> justCode
      | otherwise -> do
          hd <- mhead
          bd <- mbody
          return $
            docTypeHtml ! Attr.xmlns "http://www.w3.org/1999/xhtml" $
              hd <> bd
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
          preTagCode,
          htmlJuvixFooter,
          formattedTime
        ]

    formattedTime :: Sem r Html
    formattedTime =
      return $
        Html.span . toHtml $
          "Last modified on "
            <> formatTime
              defaultTimeLocale
              "%Y-%m-%d %-H:%M %Z"
              (o ^. genModuleHtmlArgsUTC)

    justCode :: Sem r Html
    justCode =
      ppModuleSrcHtml
        (o ^. genModuleHtmlArgsConcreteOpts)
        (o ^. genModuleHtmlArgsComments)
        (o ^. genModuleHtmlArgsModule)

    preTagCode :: Sem r Html
    preTagCode = (pre ! Attr.id "src-content") <$> justCode

    mheader :: Sem r Html
    mheader =
      return $
        Html.div ! Attr.id "package-header" $
          (Html.span ! Attr.class_ "caption" $ "")

renderTree :: (Members '[Reader HtmlOptions] r) => SimpleDocTree Ann -> Sem r Html
renderTree = go

-- | printed without comments
ppCodeHtml ::
  (PrettyPrint a, Members '[Reader HtmlOptions] r) =>
  Options ->
  a ->
  Sem r Html
ppCodeHtml opts = ppCodeHtmlHelper opts Nothing

ppModuleSrcHtml ::
  (Members '[Reader HtmlOptions] r) =>
  Options ->
  Comments ->
  Module 'Scoped 'ModuleTop ->
  Sem r Html
ppModuleSrcHtml = ppCodeHtmlComments

docToHtml :: (Members '[Reader HtmlOptions] r) => Doc Ann -> Sem r Html
docToHtml d = ppCodeHtml' <$> ask
  where
    ppCodeHtml' :: HtmlOptions -> Html
    ppCodeHtml' htmlOpts = run . runReader htmlOpts . renderTree . treeForm $ docStream'
      where
        docStream' :: SimpleDocStream Ann
        docStream' = layoutPretty defaultLayoutOptions d

ppCodeHtmlHelper ::
  (PrettyPrint a, Members '[Reader HtmlOptions] r) =>
  Options ->
  Maybe FileComments ->
  a ->
  Sem r Html
ppCodeHtmlHelper opts cs = docToHtml . docHelper cs opts

ppCodeHtmlComments ::
  (HasLoc a, PrettyPrint a, Members '[Reader HtmlOptions] r) =>
  Options ->
  Comments ->
  a ->
  Sem r Html
ppCodeHtmlComments opts cs x = ppCodeHtmlHelper opts (Just (fileComments (getLoc x ^. intervalFile) cs)) x

ppCodeHtmlInternal :: (Members '[Reader HtmlOptions] r, Internal.PrettyCode a) => a -> Sem r Html
ppCodeHtmlInternal x = do
  o <- ask
  return (ppCodeHtmlInternal' o Internal.defaultOptions x)
  where
    ppCodeHtmlInternal' :: (Internal.PrettyCode a) => HtmlOptions -> Internal.Options -> a -> Html
    ppCodeHtmlInternal' htmlOpts opts = run . runReader htmlOpts . renderTree . treeForm . docStreamInternal' opts
    docStreamInternal' :: (Internal.PrettyCode a) => Internal.Options -> a -> SimpleDocStream Ann
    docStreamInternal' opts m = layoutPretty defaultLayoutOptions (Internal.runPrettyCode opts m)

go :: (Members '[Reader HtmlOptions] r) => SimpleDocTree Ann -> Sem r Html
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

juColor :: CssColor -> Attribute
juColor = Attr.class_ . toStr
  where
    toStr :: CssColor -> AttributeValue
    toStr = \case
      JuInductive -> "ju-inductive"
      JuConstructor -> "ju-constructor"
      JuFunction -> "ju-function"
      JuComment -> "ju-comment"
      JuJudoc -> "ju-judoc"
      JuAxiom -> "ju-axiom"
      JuString -> "ju-string"
      JuKeyword -> "ju-keyword"
      JuDelimiter -> "ju-delimiter"
      JuFixity -> "ju-fixity"
      JuVar -> "ju-var"
      JuNumber -> "ju-number"

juKindColor :: S.NameKind -> CssColor
juKindColor = \case
  S.KNameConstructor -> JuConstructor
  S.KNameInductive -> JuInductive
  S.KNameFunction -> JuFunction
  S.KNameLocal -> JuVar
  S.KNameAxiom -> JuAxiom
  S.KNameLocalModule -> JuVar
  S.KNameAlias -> JuVar
  S.KNameTopModule -> JuVar
  S.KNameFixity -> JuFixity

putTag :: forall r. (Members '[Reader HtmlOptions] r) => Ann -> Html -> Sem r Html
putTag ann x = case ann of
  AnnKind k -> return (tagKind k x)
  AnnLiteralInteger -> return (Html.span ! juColor JuNumber $ x)
  AnnLiteralString -> return (Html.span ! juColor JuString $ x)
  AnnKeyword -> return (Html.span ! juColor JuKeyword $ x)
  AnnUnkindedSym -> return (Html.span ! juColor JuVar $ x)
  AnnComment -> return (Html.span ! juColor JuComment $ x)
  AnnJudoc -> return (Html.span ! juColor JuJudoc $ x)
  AnnDelimiter -> return (Html.span ! juColor JuDelimiter $ x)
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
      attrId <- nameIdAttr nid
      return $ (Html.span ! Attr.id attrId) ref'

    tagRef :: TopModulePath -> S.NameId -> Sem r Html
    tagRef tmp ni = do
      pth <- nameIdAttrRef tmp (Just ni)
      return $
        Html.span ! Attr.class_ "annot" $
          a ! Attr.href pth $
            x

    tagKind k =
      Html.span
        ! juColor (juKindColor k)

nameIdAttr :: (Members '[Reader HtmlOptions] r) => S.NameId -> Sem r AttributeValue
nameIdAttr nid = do
  pfx <- unpack <$> asks (^. htmlOptionsIdPrefix)
  return $ fromString $ pfx <> show (pretty nid)

moduleDocRelativePath :: (Members '[Reader HtmlOptions] r) => TopModulePath -> Sem r (Path Rel File)
moduleDocRelativePath m = do
  suff <- kindSuffix <$> asks (^. htmlOptionsKind)
  return (topModulePathToRelativePathDot ".html" suff m)

nameIdAttrRef :: (Members '[Reader HtmlOptions] r) => TopModulePath -> Maybe S.NameId -> Sem r AttributeValue
nameIdAttrRef tp s = do
  prefixUrl <- unpack <$> asks (^. htmlOptionsUrlPrefix)
  path <- toFilePath <$> moduleDocRelativePath tp
  noPath <- asks (^. htmlOptionsNoPath)
  let prefix = prefixUrl <> if noPath then "" else path
  attr <-
    maybe
      (return mempty)
      (((preEscapedToValue '#' <>) <$>) . nameIdAttr)
      s
  return $ fromString prefix <> attr
