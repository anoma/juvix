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
import Juvix.Data.CodeAnn (codeAnnReferenceModule, codeAnnReferenceNameId)
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
  | JuModule
  | JuFixity
  | JuNumber
  | JuComment
  | JuJudoc

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
    _genSourceHtmlArgsExt :: Text,
    _genSourceHtmlArgsStripPrefix :: Text,
    _genSourceHtmlArgsFolderStructure :: Bool,
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

genSourceHtml :: forall m. (MonadMask m, MonadIO m) => GenSourceHtmlArgs -> m ()
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
          _htmlOptionsNoPath = o ^. genSourceHtmlArgsNoPath,
          _htmlOptionsExt = o ^. genSourceHtmlArgsExt,
          _htmlOptionsStripPrefix = o ^. genSourceHtmlArgsStripPrefix,
          _htmlOptionsFolderStructure = False
        }

    entry = o ^. genSourceHtmlArgsModule

    allModules
      | _genSourceHtmlArgsNonRecursive = pure entry
      | otherwise = toList topModules

    topModules :: HashMap NameId (Module 'Scoped 'ModuleTop)
    topModules = HashMap.fromList [(entry ^. modulePath . S.nameId, entry)]

    outputModule :: Module 'Scoped 'ModuleTop -> m ()
    outputModule m = do
      ensureDir (parent outputFile)
      let absPath = (htmlOptions ^. htmlOptionsOutputDir) <//> outputFile
      putStrLn $ "Writing " <> pack (toFilePath absPath)
      utc <- liftIO getCurrentTime
      liftIO $
        Text.writeFile
          (toFilePath outputFile)
          ( run
              . runReader htmlOptions
              $ genModuleText
                GenModuleTextArgs
                  { _genModuleTextArgsConcreteOpts = o ^. genSourceHtmlArgsConcreteOpts,
                    _genModuleTextArgsUTC = utc,
                    _genModuleTextArgsComments = _genSourceHtmlArgsComments,
                    _genModuleTextArgsModule = m
                  }
          )
      where
        ext = Text.unpack (htmlOptions ^. htmlOptionsExt)

        outputFile :: Path Rel File
        outputFile = relFile (topModulePathToDottedPath (m ^. modulePath . S.nameConcrete) <.> ext)

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
      flavour <- flavourCss
      theme <- themeCss
      srcCss <- juvixSourceCss
      js <- highlightJs
      return $
        metaUtf8
          <> theme
          <> flavour
          <> srcCss
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
      return
        . Html.footer
        . Html.pre
        . toHtml
        $ "Last modified on "
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
  (PrettyPrint c, Members '[Reader HtmlOptions] r) =>
  Options ->
  c ->
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
  (PrettyPrint c, Members '[Reader HtmlOptions] r) =>
  Options ->
  Maybe FileComments ->
  c ->
  Sem r Html
ppCodeHtmlHelper opts cs = docToHtml . docHelper cs opts

ppCodeHtmlComments ::
  (HasLoc c, PrettyPrint c, Members '[Reader HtmlOptions] r) =>
  Options ->
  Comments ->
  c ->
  Sem r Html
ppCodeHtmlComments opts cs x = ppCodeHtmlHelper opts (Just (fileComments (getLoc x ^. intervalFile) cs)) x

ppCodeHtmlInternal :: (Members '[Reader HtmlOptions] r, Internal.PrettyCode c) => c -> Sem r Html
ppCodeHtmlInternal x = do
  o <- ask
  return (ppCodeHtmlInternal' o Internal.defaultOptions x)
  where
    ppCodeHtmlInternal' :: (Internal.PrettyCode c) => HtmlOptions -> Internal.Options -> c -> Html
    ppCodeHtmlInternal' htmlOpts opts = run . runReader htmlOpts . renderTree . treeForm . docStreamInternal' opts
    docStreamInternal' :: (Internal.PrettyCode c) => Internal.Options -> c -> SimpleDocStream Ann
    docStreamInternal' opts m = layoutPretty defaultLayoutOptions (Internal.runPrettyCode opts m)

go :: (Members '[Reader HtmlOptions] r) => SimpleDocTree Ann -> Sem r Html
go sdt = case sdt of
  STEmpty -> return mempty
  STChar c -> return (toHtml c)
  STText _ t -> return (toHtml t)
  STLine n -> return ("\n" <> toHtml (textSpaces n))
  STAnn ann content -> go content >>= putTag ann
  STConcat l -> mconcatMap go l
  where
    textSpaces :: Int -> Text
    textSpaces n = Text.replicate n (Text.singleton ' ')

juClass :: CssColor -> Attribute
juClass = Attr.class_ . cssColorAttribute

cssColorAttribute :: CssColor -> AttributeValue
cssColorAttribute = \case
  JuInductive -> "ju-inductive"
  JuConstructor -> "ju-constructor"
  JuFunction -> "ju-function"
  JuModule -> "ju-module"
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
  S.KNameLocalModule -> JuModule
  S.KNameAlias -> JuVar
  S.KNameTopModule -> JuModule
  S.KNameFixity -> JuFixity

putTag :: forall r. (Members '[Reader HtmlOptions] r) => Ann -> Html -> Sem r Html
putTag ann x = case ann of
  AnnKind k -> return (tagKind k x)
  AnnLiteralInteger -> return (Html.span ! juClass JuNumber $ x)
  AnnLiteralString -> return (Html.span ! juClass JuString $ x)
  AnnKeyword -> return (Html.span ! juClass JuKeyword $ x)
  AnnUnkindedSym -> return (Html.span ! juClass JuVar $ x)
  AnnComment -> return (Html.span ! juClass JuComment $ x)
  AnnPragma -> return (Html.span ! juClass JuComment $ x)
  AnnError -> return (Html.span ! juClass JuAxiom $ x)
  AnnJudoc -> return (Html.span ! juClass JuJudoc $ x)
  AnnDelimiter -> return (Html.span ! juClass JuDelimiter $ x)
  AnnDef r -> boldDefine <*> tagDef r
  AnnRef r -> tagRef r
  AnnCode -> return x
  AnnImportant -> return x
  where
    boldDefine :: Sem r (Html -> Html)
    boldDefine =
      asks (^. htmlOptionsKind) <&> \case
        HtmlDoc -> Html.span ! Attr.class_ "ju-define"
        HtmlSrc -> id
        HtmlOnly -> id

    tagDef :: CodeAnnReference -> Sem r Html
    tagDef ref = do
      ref' <- tagRef ref
      attrId <- nameIdAttr (ref ^. codeAnnReferenceNameId)
      return $ (Html.span ! Attr.id attrId) ref'

    tagRef :: CodeAnnReference -> Sem r Html
    tagRef ref = do
      pth <- nameIdAttrRef (ref ^. codeAnnReferenceModule) (Just (ref ^. codeAnnReferenceNameId))
      return
        . (Html.span ! Attr.class_ "annot")
        . ( a
              ! Attr.href pth
              ! Attr.class_ ("ju-code-link " <> cssColorAttribute (juKindColor (S.getNameKindPretty ref)))
          )
        $ x

    tagKind :: S.NameKind -> Html -> Html
    tagKind k =
      Html.span
        ! juClass (juKindColor k)

nameIdAttr :: (Members '[Reader HtmlOptions] r) => S.NameId -> Sem r AttributeValue
nameIdAttr nid = do
  pfx <- unpack <$> asks (^. htmlOptionsIdPrefix)
  return $ fromString $ pfx <> show (pretty nid)

moduleDocRelativePath :: (Members '[Reader HtmlOptions] r) => TopModulePath -> Sem r (Path Rel File)
moduleDocRelativePath m = do
  suff <- kindSuffix <$> asks (^. htmlOptionsKind)
  ext <- Text.unpack <$> asks (^. htmlOptionsExt)
  fixPrefix <- Text.unpack <$> asks (^. htmlOptionsStripPrefix)
  folderStructure <- asks (^. htmlOptionsFolderStructure)
  let pathgen :: TopModulePath -> Path Rel File
      pathgen
        | folderStructure = topModulePathToRelativePath ext suff (</>)
        | otherwise = topModulePathToRelativePathDot ext suff
  let relpath :: Path Rel File = pathgen m
  if
      | null fixPrefix -> return relpath
      | otherwise -> do
          return $
            fromMaybe
              relpath
              (stripProperPrefix (fromJust (parseRelDir fixPrefix)) relpath)

nameIdAttrRef :: (Members '[Reader HtmlOptions] r) => TopModulePath -> Maybe S.NameId -> Sem r AttributeValue
nameIdAttrRef tp mid = do
  prefixUrl <- unpack <$> asks (^. htmlOptionsUrlPrefix)
  path <- toFilePath <$> moduleDocRelativePath tp
  noPath <- asks (^. htmlOptionsNoPath)
  let prefix = prefixUrl <> if noPath then "" else path
  attr <-
    maybe
      (return mempty)
      (((preEscapedToValue '#' <>) <$>) . nameIdAttr)
      mid
  return $ fromString prefix <> attr
