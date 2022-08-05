module Juvix.Compiler.Backend.Html.Translation.FromScoped
  ( module Juvix.Compiler.Backend.Html.Translation.FromScoped,
    module Juvix.Compiler.Backend.Html.Translation.FromScoped.Generation,
    module Juvix.Compiler.Backend.Html.Data,
  )
where

import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.HashMap.Strict qualified as HashMap
import Data.Time.Clock
import Juvix.Compiler.Backend.Html.Data
import Juvix.Compiler.Backend.Html.Extra
import Juvix.Compiler.Backend.Html.Translation.FromScoped.Generation hiding (go)
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Extra
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Pretty
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Extra.Paths
import Juvix.Extra.Strings qualified as Str
import Juvix.Extra.Version
import Juvix.Prelude
import Juvix.Prelude qualified as Prelude
import Juvix.Prelude.Pretty
import Text.Blaze.Html.Renderer.Utf8 qualified as Html
-- import Data.HashMap.Strict    qualified          as HashMap
import Text.Blaze.Html5 as Html hiding (map)
import Text.Blaze.Html5.Attributes qualified as Attr

data DocParams = DocParams
  { _docParamBase :: Text,
    _docOutputDir :: FilePath
  }

makeLenses ''DocParams

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
    insertModule m@(TopModulePath ps s) t = go (Just t) (snoc ps s)
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

indexFileName :: FilePath
indexFileName = "index.html"

createIndexFile ::
  forall r.
  Members '[Reader DocParams, Embed IO, Reader HtmlOptions, Reader EntryPoint] r =>
  [TopModulePath] ->
  Sem r ()
createIndexFile ps = do
  outDir <- asks (^. docOutputDir)
  indexHtml >>= (template mempty >=> writeHtml (outDir </> indexFileName))
  where
    indexHtml :: Sem r Html
    indexHtml = do
      tree' <- root tree
      return $
        Html.div ! Attr.id "content" $
          Html.div ! Attr.id "module-list" $
            (p ! Attr.class_ "caption" $ "Modules")
              <> tree'
    tree :: ModuleTree
    tree = indexTree ps
    root :: ModuleTree -> Sem r Html
    root (Tree _ t) = do
      c' <- mconcatMapM (uncurry goChild) (HashMap.toList t)
      return $ ul c'

    goChild :: Symbol -> ModuleTree -> Sem r Html
    goChild s (Tree lbl children) = node
      where
        nodeRow :: Sem r Html
        nodeRow = case lbl of
          Nothing ->
            return $
              Html.span ! Attr.class_ attrBare $
                toHtml (prettyText s)
          Just lbl' -> do
            lnk <- nameIdAttrRef lbl' Nothing
            return $
              Html.span ! Attr.class_ attrBare $
                (a ! Attr.href lnk $ toHtml (prettyText lbl'))
        attrBase :: Html.AttributeValue
        attrBase = "details-toggle-control details-toggle collapser"
        -- dataDetailsId :: AttributeValue -> Attribute
        -- dataDetailsId = Html.dataAttribute "details-id"
        attrBare :: Html.AttributeValue
        attrBare = attrBase <> "module"
        -- attr :: Html.AttributeValue
        -- attr = case lbl of
        --   Nothing -> attrBare
        --   Just {} -> attrBase
        node :: Sem r Html
        node = do
          row' <- nodeRow
          childs' <- childs
          return (row' <>? childs')
          where
            childs :: Sem r (Maybe Html)
            childs
              | null children = return Nothing
              | otherwise = do
                  c' <- mapM (uncurry goChild) (HashMap.toList children)
                  return $
                    Just $
                      details ! Attr.open "open" $
                        -- (summary ! Attr.class_ "hide-when-js-enabled" $ "Submodules")
                        summary "Subtree"
                          <> ul (mconcatMap li c')

compileModule :: Members '[Embed IO, Reader EntryPoint, Reader NormalizedTable] r => FilePath -> Text -> Module 'Scoped 'ModuleTop -> Sem r ()
compileModule dir baseName m = runReader params $ do
  copyAssets
  mapM_ goTopModule topModules
  runReader docHtmlOpts (createIndexFile (map topModulePath (toList topModules)))
  where
    copyAssets :: forall s. Members '[Embed IO, Reader DocParams] s => Sem s ()
    copyAssets = do
      toAssetsDir <- (</> "assets") <$> asks (^. docOutputDir)
      let writeAsset :: (FilePath, BS.ByteString) -> Sem s ()
          writeAsset (filePath, fileContents) =
            Prelude.embed $ BS.writeFile (toAssetsDir </> takeFileName filePath) fileContents
      Prelude.embed (createDirectoryIfMissing True toAssetsDir)
      mapM_ writeAsset assetFiles
      where
        assetFiles :: [(FilePath, BS.ByteString)]
        assetFiles = $(assetsDir)

    params :: DocParams
    params =
      DocParams
        { _docParamBase = baseName,
          _docOutputDir = dir
        }
    topModules :: HashMap NameId (Module 'Scoped 'ModuleTop)
    topModules = getAllModules m

writeHtml :: Members '[Embed IO] r => FilePath -> Html -> Sem r ()
writeHtml f h = Prelude.embed $ do
  createDirectoryIfMissing True dir
  Builder.writeFile f (Html.renderHtmlBuilder h)
  where
    dir :: FilePath
    dir = takeDirectory f

moduleDocPath :: Members '[Reader HtmlOptions, Reader DocParams] r => Module 'Scoped 'ModuleTop -> Sem r FilePath
moduleDocPath m = do
  relPath <- moduleDocRelativePath (m ^. modulePath . S.nameConcrete)
  outDir <- asks (^. docOutputDir)
  return (outDir </> relPath)

topModulePath ::
  Module 'Scoped 'ModuleTop -> TopModulePath
topModulePath = (^. modulePath . S.nameConcrete)

srcHtmlOpts :: HtmlOptions
srcHtmlOpts = HtmlOptions HtmlSrc

docHtmlOpts :: HtmlOptions
docHtmlOpts = HtmlOptions HtmlDoc

template :: forall r. Members '[Reader EntryPoint] r => Html -> Html -> Sem r Html
template rightMenu' content' = do
  body' <- mbody
  return (docTypeHtml (mhead <> body'))
  where
    mhead :: Html
    mhead =
      Html.head $
        title titleStr
          <> Html.meta
            ! Attr.httpEquiv "Content-Type"
            ! Attr.content "text/html; charset=UTF-8"
          <> Html.meta
            ! Attr.name "viewport"
            ! Attr.content "width=device-width, initial-scale=1"
          <> mathJaxCdn
          -- <> highlightJs
          <> livejs
          <> ayuCss
          <> linuwialCss

    titleStr :: Html
    titleStr = "Juvix Documentation"

    packageHeader :: Sem r Html
    packageHeader = do
      pkgName' <- toHtml <$> asks (^. entryPointPackage . packageName')
      version' <- toHtml <$> asks (^. entryPointPackage . packageVersion')
      return $
        Html.div ! Attr.id "package-header" $
          ( Html.span ! Attr.class_ "caption" $
              pkgName' <> " - " <> version'
          )
            <> rightMenu'

    mbody :: Sem r Html
    mbody = do
      bodyHeader' <- packageHeader
      return $
        body ! Attr.class_ "js-enabled" $
          bodyHeader'
            <> content'
            <> mfooter

    mfooter :: Html
    mfooter =
      Html.div ! Attr.id "footer" $
        p
          ( "Build by "
              <> (Html.a ! Attr.href Str.juvixDotOrg $ "Juvix")
              <> " version "
              <> toHtml versionDoc
          )
          <> ( Html.a ! Attr.href Str.juvixDotOrg $
                 Html.img
                   ! Attr.id "tara"
                   ! Attr.src "assets/Seating_Tara_smiling.svg"
                   ! Attr.alt "Tara"
             )

-- | This function compiles a datalang module into Html documentation.
goTopModule ::
  forall r.
  Members '[Reader DocParams, Embed IO, Reader EntryPoint, Reader NormalizedTable] r =>
  Module 'Scoped 'ModuleTop ->
  Sem r ()
goTopModule m = do
  runReader docHtmlOpts $ do
    fpath <- moduleDocPath m
    Prelude.embed (putStrLn ("processing " <> pack fpath))
    docHtml >>= writeHtml fpath

  runReader srcHtmlOpts $ do
    fpath <- moduleDocPath m
    srcHtml >>= writeHtml fpath
  where
    tmp :: TopModulePath
    tmp = m ^. modulePath . S.nameConcrete

    srcHtml :: forall s. Members '[Reader HtmlOptions, Embed IO] s => Sem s Html
    srcHtml = do
      utc <- Prelude.embed getCurrentTime
      return (genModuleHtml defaultOptions HtmlSrc True utc Ayu m)

    docHtml :: forall s. Members '[Reader HtmlOptions, Reader EntryPoint, Reader NormalizedTable] s => Sem s Html
    docHtml = do
      content' <- content
      rightMenu' <- rightMenu
      template rightMenu' content'
      where
        rightMenu :: Sem s Html
        rightMenu = do
          sourceRef' <- local (set htmlOptionsKind HtmlSrc) (nameIdAttrRef tmp Nothing)
          return $
            ul ! Attr.id "page-menu" ! Attr.class_ "links" $
              li (a ! Attr.href sourceRef' $ "Source")
                <> li (a ! Attr.href (fromString indexFileName) $ "Index")

        content :: Sem s Html
        content = do
          preface' <- docPreface
          interface' <- interface
          return $
            Html.div ! Attr.id "content" $
              moduleHeader
                <> toc
                <> preface'
                -- <> synopsis
                <> interface'

        docPreface :: Sem s Html
        docPreface = do
          pref <- goJudocMay (m ^. moduleDoc)
          return $
            Html.div ! Attr.id "description" $
              Html.div ! Attr.class_ "doc" $
                ( a ! Attr.id "sec:description" ! Attr.href "sec:description" $
                    h1 "Description"
                )
                  <> pref

        toc :: Html
        toc =
          Html.div ! Attr.id "table-of-contents" $
            Html.div ! Attr.id "contents-list" $
              ( p
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
          Html.div ! Attr.id "module-header" $
            (p ! Attr.class_ "caption" $ toHtml (prettyText tmp))

        interface :: Sem s Html
        interface = do
          sigs' <- mconcatMapM goStatement (m ^. moduleBody)
          return $
            Html.div ! Attr.id "interface" $
              ( a ! Attr.id "sec:interface" ! Attr.href "sec:interface" $
                  h1 "Definitions"
              )
                <> sigs'

goJudocMay :: Members '[Reader HtmlOptions, Reader NormalizedTable] r => Maybe (Judoc 'Scoped) -> Sem r Html
goJudocMay = maybe (return mempty) goJudoc

goJudoc :: forall r. Members '[Reader HtmlOptions, Reader NormalizedTable] r => Judoc 'Scoped -> Sem r Html
goJudoc (Judoc bs) = mconcatMapM goBlock bs
  where
    goBlock :: JudocBlock 'Scoped -> Sem r Html
    goBlock = \case
      JudocParagraph ls -> mconcatMapM goLine (toList ls)
      JudocExample e -> goExample e
    goLine :: JudocParagraphLine 'Scoped -> Sem r Html
    goLine (JudocParagraphLine atoms) = mconcatMapM goAtom (toList atoms)
    goExample :: Example 'Scoped -> Sem r Html
    goExample ex = do
      e' <- ppCodeHtml (ex ^. exampleExpression)
      norm' <- asks @NormalizedTable (^?! at (ex ^. exampleId) . _Just) >>= ppCodeHtmlInternal
      return $
        Html.pre ! Attr.class_ "screen" $
          (Html.code ! Attr.class_ "prompt" $ Str.judocExample)
            <> " "
            <> e'
            <> "\n"
            <> norm'
    goAtom :: JudocAtom 'Scoped -> Sem r Html
    goAtom = \case
      JudocExpression e -> ppCodeHtml e
      JudocText txt -> return (toHtml txt)

goStatement :: Members '[Reader HtmlOptions, Reader NormalizedTable] r => Statement 'Scoped -> Sem r Html
goStatement = \case
  StatementTypeSignature t -> goTypeSignature t
  StatementAxiom t -> goAxiom t
  StatementInductive t -> goInductive t
  StatementOpenModule t -> goOpen t
  _ -> mempty

goOpen :: forall r. Members '[Reader HtmlOptions] r => OpenModule 'Scoped -> Sem r Html
goOpen op
  | Public <- op ^. openPublic = noDefHeader <$> ppCodeHtml op
  | otherwise = mempty

goAxiom :: forall r. Members '[Reader HtmlOptions, Reader NormalizedTable] r => AxiomDef 'Scoped -> Sem r Html
goAxiom axiom = do
  header' <- axiomHeader
  defHeader tmp uid header' Nothing
  where
    uid :: NameId
    uid = axiom ^. axiomName . S.nameId
    tmp :: TopModulePath
    tmp = axiom ^. axiomName . S.nameDefinedIn . S.absTopModulePath
    axiomHeader :: Sem r Html
    axiomHeader = ppCodeHtml (set axiomDoc Nothing axiom)

goInductive :: forall r. Members '[Reader HtmlOptions, Reader NormalizedTable] r => InductiveDef 'Scoped -> Sem r Html
goInductive def = do
  sig' <- inductiveHeader
  header' <- defHeader tmp uid sig' (def ^. inductiveDoc)
  body' <- goConstructors (def ^. inductiveConstructors)
  return (header' <> body')
  where
    uid :: NameId
    uid = def ^. inductiveName . S.nameId
    tmp :: TopModulePath
    tmp = def ^. inductiveName . S.nameDefinedIn . S.absTopModulePath
    inductiveHeader :: Sem r Html
    inductiveHeader =
      runReader defaultOptions (ppInductiveSignature def) >>= ppCodeHtml

goConstructors :: forall r. Members '[Reader HtmlOptions, Reader NormalizedTable] r => [InductiveConstructorDef 'Scoped] -> Sem r Html
goConstructors cc = do
  tbl' <- table . tbody <$> mconcatMapM goConstructor cc
  return $
    Html.div ! Attr.class_ "subs constructors" $
      (p ! Attr.class_ "caption" $ "Constructors")
        <> tbl'
  where
    goConstructor :: InductiveConstructorDef 'Scoped -> Sem r Html
    goConstructor c = do
      src' <- srcPart
      doc' <- docPart
      return (tr (src' <> doc'))
      where
        docPart :: Sem r Html
        docPart = do
          td ! Attr.class_ "doc"
            <$> goJudocMay (c ^. constructorDoc)

        srcPart :: Sem r Html
        srcPart = do
          sig' <- ppCodeHtml (set constructorDoc Nothing c)
          return $
            td ! Attr.class_ "src" $
              sig'

noDefHeader :: Html -> Html
noDefHeader = p ! Attr.class_ "src"

defHeader :: forall r. Members '[Reader HtmlOptions, Reader NormalizedTable] r => TopModulePath -> NameId -> Html -> Maybe (Judoc 'Scoped) -> Sem r Html
defHeader tmp uid sig mjudoc = do
  funHeader' <- functionHeader
  judoc' <- judoc
  return $
    Html.div ! Attr.class_ "top" $
      funHeader'
        <> judoc'
  where
    judoc :: Sem r Html
    judoc = do
      judoc' <- goJudocMay mjudoc
      return (Html.div ! Attr.class_ "doc" $ judoc')

    functionHeader :: Sem r Html
    functionHeader = do
      sourceLink' <- sourceAndSelfLink tmp uid
      return $ noDefHeader (sig <> sourceLink')

goTypeSignature :: forall r. Members '[Reader HtmlOptions, Reader NormalizedTable] r => TypeSignature 'Scoped -> Sem r Html
goTypeSignature sig = do
  sig' <- typeSig
  defHeader tmp uid sig' (sig ^. sigDoc)
  where
    tmp :: TopModulePath
    tmp = sig ^. sigName . S.nameDefinedIn . S.absTopModulePath
    uid :: NameId
    uid = sig ^. sigName . S.nameId
    typeSig :: Sem r Html
    typeSig = ppCodeHtml (set sigDoc Nothing sig)

sourceAndSelfLink :: Members '[Reader HtmlOptions] r => TopModulePath -> NameId -> Sem r Html
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

tagIden :: IsString a => NameId -> a
tagIden x = fromText $ prettyText x

selfLinkName :: IsString a => NameId -> a
selfLinkName x = fromText $ "#" <> tagIden x
