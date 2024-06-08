module Juvix.Compiler.Backend.Html.Translation.FromTyped
  ( module Juvix.Compiler.Backend.Html.Translation.FromTyped,
    module Juvix.Compiler.Backend.Html.Translation.FromTyped.Source,
    module Juvix.Compiler.Backend.Html.Data,
  )
where

import Data.ByteString.Builder qualified as Builder
import Data.HashMap.Strict qualified as HashMap
import Data.Time.Clock
import Data.Versions (prettySemVer)
import Juvix.Compiler.Backend.Html.Data
import Juvix.Compiler.Backend.Html.Extra
import Juvix.Compiler.Backend.Html.Translation.FromTyped.Source hiding (go)
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Print
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Extra.Assets
import Juvix.Prelude
import Juvix.Prelude.Pretty
import Text.Blaze.Html.Renderer.Utf8 qualified as Html
import Text.Blaze.Html5 as Html hiding (map)
import Text.Blaze.Html5.Attributes qualified as Attr

data JudocCtx = JudocCtx
  { _judocCtxComments :: Comments,
    _judocCtxTopModules :: [Module 'Scoped 'ModuleTop]
  }

data JudocArgs = JudocArgs
  { _judocArgsOutputDir :: Path Abs Dir,
    _judocArgsBaseName :: Text,
    _judocArgsCtx :: JudocCtx,
    _judocArgsMainModule :: Module 'Scoped 'ModuleTop,
    _judocArgsAssetsPrefix :: Text,
    _judocArgsUrlPrefix :: Text,
    _judocArgsTheme :: Theme,
    _judocArgsNonRecursive :: Bool,
    _judocArgsNoFooter :: Bool,
    _judocArgsIdPrefix :: Text,
    _judocArgsNoPath :: Bool,
    _judocArgsExt :: Text,
    _judocArgsStripPrefix :: Text,
    _judocArgsFolderStructure :: Bool
  }

makeLenses ''JudocCtx
makeLenses ''JudocArgs

instance Semigroup JudocCtx where
  ctx1 <> ctx2 =
    JudocCtx
      { _judocCtxComments = ctx1 ^. judocCtxComments <> ctx2 ^. judocCtxComments,
        _judocCtxTopModules = ctx1 ^. judocCtxTopModules <> ctx2 ^. judocCtxTopModules
      }

instance Monoid JudocCtx where
  mempty =
    JudocCtx
      { _judocCtxComments = mempty,
        _judocCtxTopModules = mempty
      }

data Tree k a = Tree
  { _treeLabel :: a,
    _treeChildren :: HashMap k (Tree k a)
  }

makeLenses ''Tree

type ModuleTree = Tree Symbol (Maybe TopModulePath)

indexTree :: [TopModulePath] -> Tree Symbol (Maybe TopModulePath)
indexTree = foldr insertModule emptyTree
  where
    insertModule :: TopModulePath -> ModuleTree -> ModuleTree
    insertModule m@(TopModulePath ps name) t = go (Just t) (snoc ps name)
      where
        go :: Maybe ModuleTree -> [Symbol] -> ModuleTree
        go tree = \case
          [] -> set treeLabel (Just m) t'
          (k : ks) -> over (treeChildren . at k) (Just . flip go ks) t'
          where
            t' :: ModuleTree
            t' = fromMaybe emptyTree tree
    emptyTree :: Tree Symbol (Maybe TopModulePath)
    emptyTree = Tree Nothing mempty

indexFileName :: Path Rel File
indexFileName = $(mkRelFile "index.html")

createIndexFile ::
  forall r.
  (Members '[EmbedIO, Reader HtmlOptions, Reader EntryPoint] r) =>
  [TopModulePath] ->
  Sem r ()
createIndexFile ps = do
  outputDir <- asks (^. htmlOptionsOutputDir)
  indexHtml >>= (topTemplate mempty >=> writeHtml (outputDir <//> indexFileName))
  where
    indexHtml :: Sem r Html
    indexHtml = do
      tree' <- root tree
      return
        $ Html.div
          ! Attr.id "content"
        $ Html.div
          ! Attr.id "module-list"
        $ (p ! Attr.class_ "caption" $ "Modules")
          <> ( button
                 ! Attr.id "toggle-all-button"
                 ! Attr.class_ "toggle-button opened"
                 ! Attr.onclick "toggle()"
                 $ Html.span
                   ! Attr.id "toggle-button-text"
                   ! Attr.class_ "toggle-icon"
                 $ "▼ Hide all modules"
             )
          <> tree'

    tree :: ModuleTree
    tree = indexTree ps

    root :: ModuleTree -> Sem r Html
    root (Tree _ t) = do
      c' <- mconcatMapM (uncurry goChild) (HashMap.toList t)
      return $ ul c'

    goChild :: Symbol -> ModuleTree -> Sem r Html
    goChild sym (Tree lbl children) = node
      where
        nodeRow :: Sem r Html
        nodeRow = case lbl of
          Nothing ->
            return
              $ Html.span
                ! Attr.class_ attrBare
              $ toHtml (prettyText sym)
          Just lbl' -> do
            lnk <- nameIdAttrRef lbl' Nothing
            return
              $ Html.span
                ! Attr.class_ attrBare
              $ (a ! Attr.href lnk $ toHtml (prettyText lbl'))

        attrBase :: Html.AttributeValue
        attrBase = "details-toggle-control details-toggle"

        attrBare :: Html.AttributeValue
        attrBare = attrBase <> " directory"

        node :: Sem r Html
        node = nodeRow >>= getRowWithChilds
          where
            getRowWithChilds :: Html -> Sem r Html
            getRowWithChilds row'
              | null children = return row'
              | otherwise = do
                  c' <- mapM (uncurry goChild) (HashMap.toList children)
                  return
                    $ details
                      ! Attr.open "open"
                    $ summary row'
                      <> ul (mconcatMap li c')

writeHtml :: (Members '[EmbedIO] r) => Path Abs File -> Html -> Sem r ()
writeHtml f h = liftIO $ do
  ensureDir dir
  Builder.writeFile (toFilePath f) (Html.renderHtmlBuilder h)
  where
    dir :: Path Abs Dir
    dir = parent f

genJudocHtml :: (Members '[EmbedIO] r) => EntryPoint -> JudocArgs -> Sem r ()
genJudocHtml entry JudocArgs {..} =
  runReader htmlOpts . runReader entry $ do
    liftIO (writeAssets _judocArgsOutputDir)
    mapM_ (goTopModule cs) allModules
    createIndexFile (map topModulePath (toList allModules))
  where
    cs :: Comments
    cs = _judocArgsCtx ^. judocCtxComments

    mainMod :: Module 'Scoped 'ModuleTop
    mainMod = _judocArgsMainModule

    htmlOpts :: HtmlOptions
    htmlOpts =
      HtmlOptions
        { _htmlOptionsKind = HtmlDoc,
          _htmlOptionsAssetsPrefix = _judocArgsAssetsPrefix,
          _htmlOptionsOutputDir = _judocArgsOutputDir,
          _htmlOptionsUrlPrefix = _judocArgsUrlPrefix,
          _htmlOptionsIdPrefix = _judocArgsIdPrefix,
          _htmlOptionsParamBase = _judocArgsBaseName,
          _htmlOptionsTheme = _judocArgsTheme,
          _htmlOptionsNoFooter = _judocArgsNoFooter,
          _htmlOptionsOnlyCode = False,
          _htmlOptionsNoPath = _judocArgsNoPath,
          _htmlOptionsExt = _judocArgsExt,
          _htmlOptionsStripPrefix = _judocArgsStripPrefix,
          _htmlOptionsFolderStructure = _judocArgsFolderStructure
        }

    allModules
      | _judocArgsNonRecursive = pure mainMod
      | otherwise = toList topModules

    topModules :: [Module 'Scoped 'ModuleTop]
    topModules = _judocArgsCtx ^. judocCtxTopModules

moduleDocPath :: (Members '[Reader HtmlOptions] r) => Module 'Scoped 'ModuleTop -> Sem r (Path Abs File)
moduleDocPath m = do
  relPath <- moduleDocRelativePath (m ^. modulePath . S.nameConcrete)
  outputDir <- asks (^. htmlOptionsOutputDir)
  return (outputDir <//> relPath)

topModulePath :: Module 'Scoped 'ModuleTop -> TopModulePath
topModulePath = (^. modulePath . S.nameConcrete)

topTemplate :: forall r. (Members '[Reader EntryPoint, Reader HtmlOptions] r) => Html -> Html -> Sem r Html
topTemplate rightMenu' content' = do
  mathJax <- mathJaxCdn
  ayuTheme <- ayuCss
  judocTheme <- linuwialCss
  toggle <- toggleJs
  let mhead :: Html
      mhead =
        Html.head $
          title titleStr
            <> Html.meta
              ! Attr.httpEquiv "Content-Type"
              ! Attr.content "text/html; charset=UTF-8"
            <> Html.meta
              ! Attr.name "viewport"
              ! Attr.content "width=device-width, initial-scale=1"
            <> mathJax
            <> livejs
            <> ayuTheme
            <> judocTheme
            <> toggle

      titleStr :: Html
      titleStr = "Juvix Documentation"

      packageHeader :: Sem r Html
      packageHeader = do
        pkgName' <- toHtml <$> asks (^. entryPointPackage . packageName)
        version' <- toHtml <$> asks (^. entryPointPackage . packageVersion . to prettySemVer)
        return
          $ Html.div
            ! Attr.id "package-header"
          $ ( Html.span
                ! Attr.class_ "caption"
                $ pkgName'
                  <> " - "
                  <> version'
            )
            <> rightMenu'

      mbody :: Sem r Html
      mbody = do
        bodyHeader' <- packageHeader
        footer' <- htmlJuvixFooter
        return
          $ body
            ! Attr.class_ "js-enabled"
          $ bodyHeader'
            <> content'
            <> footer'

  body' <- mbody
  return $ docTypeHtml (mhead <> body')

-- | This function compiles a module into Html documentation.
goTopModule ::
  forall r.
  (Members '[Reader HtmlOptions, EmbedIO, Reader EntryPoint] r) =>
  Comments ->
  Module 'Scoped 'ModuleTop ->
  Sem r ()
goTopModule cs m = do
  htmlOpts <- ask @HtmlOptions
  runReader (htmlOpts {_htmlOptionsKind = HtmlDoc}) $ do
    fpath <- moduleDocPath m
    putStrLn ("Writing " <> pack (toFilePath fpath))
    docHtml >>= writeHtml fpath

  runReader (htmlOpts {_htmlOptionsKind = HtmlSrc}) $ do
    fpath <- moduleDocPath m
    srcHtml >>= writeHtml fpath
  where
    tmp :: TopModulePath
    tmp = m ^. modulePath . S.nameConcrete

    srcHtml :: forall s. (Members '[Reader HtmlOptions, EmbedIO] s) => Sem s Html
    srcHtml = do
      utc <- liftIO getCurrentTime
      genModuleHtml
        GenModuleHtmlArgs
          { _genModuleHtmlArgsConcreteOpts = defaultOptions,
            _genModuleHtmlArgsUTC = utc,
            _genModuleHtmlArgsComments = cs,
            _genModuleHtmlArgsModule = m
          }

    docHtml :: forall s. (Members '[Reader HtmlOptions, Reader EntryPoint] s) => Sem s Html
    docHtml = do
      content' <- content
      rightMenu' <- rightMenu
      topTemplate rightMenu' content'
      where
        rightMenu :: Sem s Html
        rightMenu = do
          sourceRef' <- local (set htmlOptionsKind HtmlSrc) (nameIdAttrRef tmp Nothing)
          return
            $ ul
              ! Attr.id "page-menu"
              ! Attr.class_ "links"
            $ li (a ! Attr.href sourceRef' $ "Source")
              <> li (a ! Attr.href (fromString (toFilePath indexFileName)) $ "Index")

        content :: Sem s Html
        content = do
          preface' <- docPreface
          interface' <- moduleInterface
          return
            $ Html.div
              ! Attr.id "content"
            $ moduleHeader
              <> toc
              <> preface'
              -- <> synopsis
              <> interface'

        docPreface :: Sem s Html
        docPreface = do
          pref <- goJudocMay (m ^. moduleDoc)
          return
            $ Html.div
              ! Attr.id "description"
            $ Html.div
              ! Attr.class_ "doc"
            $ ( a
                  ! Attr.id "sec:description"
                  ! Attr.href "sec:description"
                  $ h1 "Description"
              )
              <> pref

        toc :: Html
        toc =
          Html.div
            ! Attr.id "table-of-contents"
            $ Html.div
              ! Attr.id "contents-list"
            $ ( p
                  ! Attr.class_ "caption"
                  ! Attr.onclick "window.scrollTo(0,0)"
                  $ "Contents"
              )
              <> tocEntries
          where
            tocEntries :: Html
            tocEntries =
              ul $
                li (a ! Attr.href "#sec:description" $ "Description")
                  <> li (a ! Attr.href "#sec:interface" $ "Definitions")

        moduleHeader :: Html
        moduleHeader =
          Html.div
            ! Attr.id "module-header"
            $ (p ! Attr.class_ "caption" $ toHtml (prettyText tmp))

        moduleInterface :: Sem s Html
        moduleInterface = do
          sigs' <- mconcatMapM goStatement (m ^. moduleBody)
          return
            $ Html.div
              ! Attr.id "interface"
            $ ( a
                  ! Attr.id "sec:interface"
                  ! Attr.href "sec:interface"
                  $ h1 "Definitions"
              )
              <> sigs'

goJudocMay :: (Members '[Reader HtmlOptions] r) => Maybe (Judoc 'Scoped) -> Sem r Html
goJudocMay = maybe (return mempty) goJudoc

goJudoc :: forall r. (Members '[Reader HtmlOptions] r) => Judoc 'Scoped -> Sem r Html
goJudoc (Judoc bs) = mconcatMapM goGroup bs
  where
    goGroup :: JudocGroup 'Scoped -> Sem r Html
    goGroup = \case
      JudocGroupLines l -> mconcatMapM goBlock l
      JudocGroupBlock l -> goGroupBlock l

    goGroupBlock :: JudocBlockParagraph 'Scoped -> Sem r Html
    goGroupBlock = mconcatMap goBlock . (^. judocBlockParagraphBlocks)

    goBlock :: JudocBlock 'Scoped -> Sem r Html
    goBlock = \case
      JudocLines ls -> Html.p . concatWith (\l r -> l <> " " <> r) <$> mapM goLine (toList ls)

    goLine :: JudocLine 'Scoped -> Sem r Html
    goLine (JudocLine _ atoms) = mconcatMapM goAtom (fmap (^. withLocParam) atoms)

    goAtom :: JudocAtom 'Scoped -> Sem r Html
    goAtom = \case
      JudocExpression e -> ppCodeHtml defaultOptions e
      JudocText txt -> return (toHtml txt)

goStatement :: forall r. (Members '[Reader HtmlOptions] r) => Statement 'Scoped -> Sem r Html
goStatement = \case
  StatementAxiom t -> goAxiom t
  StatementInductive t -> goInductive t
  StatementOpenModule t -> goOpen t
  StatementFunctionDef t -> goFunctionDef t
  StatementSyntax syn -> goSyntax syn
  StatementImport t -> goImport t
  StatementModule m -> goLocalModule m
  StatementProjectionDef {} -> mempty
  where
    goSyntax :: SyntaxDef 'Scoped -> Sem r Html
    goSyntax = \case
      SyntaxFixity f -> goFixity f
      SyntaxAlias d -> goAlias d
      SyntaxOperator {} -> mempty
      SyntaxIterator {} -> mempty

goFixity :: forall r. (Members '[Reader HtmlOptions] r) => FixitySyntaxDef 'Scoped -> Sem r Html
goFixity def = do
  sig' <- ppHelper (ppFixityDefHeaderNew def)
  header' <- defHeader (def ^. fixitySymbol) sig' (def ^. fixityDoc)
  prec' <- mkPrec
  let tbl' = table . tbody $ ari <> prec'
  return $
    header'
      <> ( Html.div
             ! Attr.class_ "subs"
             $ (p ! Attr.class_ "caption" $ "Fixity details")
               <> tbl'
         )
  where
    info :: ParsedFixityInfo 'Scoped
    info = def ^. fixityInfo

    row :: Html -> Html
    row x = tr $ td ! Attr.class_ "src" $ x

    mkPrec :: Sem r Html
    mkPrec = case info ^. fixityPrecSame of
      Just txt -> do
        d <- ppCodeHtml defaultOptions txt
        return (row $ toHtml ("Same precedence as " <> d))
      Nothing ->
        goPrec "Higher" (info ^. fixityPrecAbove)
          <> goPrec "Lower" (info ^. fixityPrecBelow)
        where
          goPrec :: Html -> Maybe [S.Symbol] -> Sem r Html
          goPrec above ls = case ls >>= nonEmpty of
            Nothing -> mempty
            Just l -> do
              l' <- foldr (\x acc -> x <> ", " <> acc) mempty <$> mapM (ppCodeHtml defaultOptions) l
              return (row $ above <> " precedence than: " <> l')

    ari :: Html
    ari =
      let arit = toHtml @String $ show (info ^. fixityParsedArity)
          assoc = toHtml @String $ case fromMaybe AssocNone (info ^. fixityAssoc) of
            AssocNone -> ""
            AssocRight -> ", right-associative"
            AssocLeft -> ", left-associative"
       in row $
            arit
              <> assoc

goAlias :: forall r. (Members '[Reader HtmlOptions] r) => AliasDef 'Scoped -> Sem r Html
goAlias def = do
  sig' <- ppCodeHtml defaultOptions def
  defHeader (def ^. aliasDefName) sig' Nothing

-- | local modules generated by inductive types should not show up.
goLocalModule :: forall r. (Members '[Reader HtmlOptions] r) => Module 'Scoped 'ModuleLocal -> Sem r Html
goLocalModule def = fmap (fromMaybe mempty) . runFail $ do
  failWhen (def ^. moduleInductive)
  sig' <- ppHelper (ppModuleHeader def)
  header' <- defHeader (def ^. modulePath) sig' (def ^. moduleDoc)
  body' <-
    ( Html.div
        ! Attr.class_ "subs"
      )
      <$> mconcatMap goStatement (def ^. moduleBody)
  return (header' <> body')

goImport :: forall r. (Members '[Reader HtmlOptions] r) => Import 'Scoped -> Sem r Html
goImport op
  | Just Public {} <- op ^? importOpen . _Just . openPublic = noDefHeader <$> ppCodeHtml defaultOptions op
  | otherwise = mempty

goOpen :: forall r. (Members '[Reader HtmlOptions] r) => OpenModule 'Scoped -> Sem r Html
goOpen op
  | Public {} <- op ^. openModuleParams . openPublic = noDefHeader <$> ppCodeHtml defaultOptions op
  | otherwise = mempty

goAxiom :: forall r. (Members '[Reader HtmlOptions] r) => AxiomDef 'Scoped -> Sem r Html
goAxiom axiom = do
  header' <- axiomHeader
  defHeader (axiom ^. axiomName) header' (axiom ^. axiomDoc)
  where
    axiomHeader :: Sem r Html
    axiomHeader = ppCodeHtml defaultOptions (set axiomDoc Nothing axiom)

goFunctionDef :: forall r. (Members '[Reader HtmlOptions] r) => FunctionDef 'Scoped -> Sem r Html
goFunctionDef def = do
  sig' <- funSig
  defHeader (def ^. signName) sig' (def ^. signDoc)
  where
    funSig :: Sem r Html
    funSig = ppHelper (ppFunctionSignature def)

goInductive :: forall r. (Members '[Reader HtmlOptions] r) => InductiveDef 'Scoped -> Sem r Html
goInductive def = do
  sig' <- inductiveHeader
  header' <- defHeader (def ^. inductiveName) sig' (def ^. inductiveDoc)
  body' <- goConstructors (def ^. inductiveConstructors)
  return (header' <> body')
  where
    inductiveHeader :: Sem r Html
    inductiveHeader = ppHelper (ppInductiveSignature def)

ppHelper :: (Members '[Reader HtmlOptions] r) => Sem '[ExactPrint, Reader Options] () -> Sem r Html
ppHelper = docToHtml . run . runReader defaultOptions . execExactPrint Nothing

goConstructors :: forall r. (Members '[Reader HtmlOptions] r) => NonEmpty (ConstructorDef 'Scoped) -> Sem r Html
goConstructors cc = do
  tbl' <- table . tbody <$> mconcatMapM goConstructor cc
  return
    $ Html.div
      ! Attr.class_ "subs constructors"
    $ (p ! Attr.class_ "caption" $ "Constructors")
      <> tbl'
  where
    goConstructor :: ConstructorDef 'Scoped -> Sem r Html
    goConstructor c = do
      src' <- srcPart
      doc' <- docPart
      return (tr (src' <> doc'))
      where
        docPart :: Sem r Html
        docPart = do
          td
            ! Attr.class_ "doc"
            <$> goJudocMay (c ^. constructorDoc)

        srcPart :: Sem r Html
        srcPart = do
          sig' <- ppHelper (ppConstructorDef False (set constructorDoc Nothing c))
          return
            $ td
              ! Attr.class_ "src"
            $ sig'

noDefHeader :: Html -> Html
noDefHeader = p ! Attr.class_ "src"

defHeader ::
  forall r x.
  (Members '[Reader HtmlOptions] r) =>
  S.Name' x ->
  Html ->
  Maybe (Judoc 'Scoped) ->
  Sem r Html
defHeader name sig mjudoc = do
  funHeader' <- functionHeader
  judoc' <- judoc
  return
    $ Html.div
      ! Attr.class_ "top"
    $ funHeader'
      <> judoc'
  where
    uid :: NameId
    uid = name ^. S.nameId

    tmp :: TopModulePath
    tmp = name ^. S.nameDefinedIn . S.absTopModulePath

    judoc :: Sem r Html
    judoc = do
      judoc' <- goJudocMay mjudoc
      return (Html.div ! Attr.class_ "doc" $ judoc')

    functionHeader :: Sem r Html
    functionHeader = do
      sourceLink' <- sourceAndSelfLink tmp uid
      return $ noDefHeader (sig <> sourceLink')

sourceAndSelfLink :: (Members '[Reader HtmlOptions] r) => TopModulePath -> NameId -> Sem r Html
sourceAndSelfLink tmp name = do
  ref' <- local (set htmlOptionsKind HtmlSrc) (nameIdAttrRef tmp (Just name))
  return $
    ( a
        ! Attr.href ref'
        ! Attr.class_ "link"
        $ "Source"
    )
      <> ( a
             ! Attr.href (selfLinkName name)
             ! Attr.class_ "selflink"
             $ "#"
         )

tagIden :: (IsString c) => NameId -> c
tagIden = fromText . prettyText

selfLinkName :: (IsString c) => NameId -> c
selfLinkName x = fromText $ "#" <> tagIden x
