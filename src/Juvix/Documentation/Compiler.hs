module Juvix.Documentation.Compiler where

import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.Time.Clock
import Juvix.Documentation.Extra
import Juvix.Prelude
import Juvix.Prelude qualified as Prelude
import Juvix.Prelude.Html
import Juvix.Prelude.Pretty
import Juvix.Syntax.Concrete.Language
import Juvix.Syntax.Concrete.Scoped.Name qualified as S
import Juvix.Syntax.Concrete.Scoped.Pretty
import Juvix.Syntax.Concrete.Scoped.Pretty.Html
import Juvix.Syntax.Concrete.Scoped.Utils
import Juvix.Syntax.NameId
import Juvix.Utils.Paths
import Text.Blaze.Html.Renderer.Utf8 qualified as Html
-- import Data.HashMap.Strict    qualified          as HashMap
import Text.Blaze.Html5 as Html hiding (map)
import Text.Blaze.Html5.Attributes qualified as Attr

data DocParams = DocParams
  { _docParamBase :: Text,
    _docOutputDir :: FilePath
  }

makeLenses ''DocParams

compileModuleHtmlText :: Members '[Embed IO] r => FilePath -> Text -> Module 'Scoped 'ModuleTop -> Sem r ()
compileModuleHtmlText dir baseName m =
  runReader params (mapM_ goTopModule topModules)
  where
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

-- | This function compiles a datalang module into Html documentation.
goTopModule ::
  forall r.
  Members '[Reader DocParams, Embed IO] r =>
  Module 'Scoped 'ModuleTop ->
  Sem r ()
goTopModule m = do
  copyAssets
  runReader htmlOpts $ do
    fpath <- moduleDocPath m
    Prelude.embed (putStrLn ("processing " <> pack fpath))
    docHtml >>= writeHtml fpath

  runReader srcHtmlOpts $ do
    fpath <- moduleDocPath m
    srcHtml >>= writeHtml fpath
  where
    copyAssets :: Sem r ()
    copyAssets = do
      toAssetsDir <- (</> "assets") <$> asks (^. docOutputDir)
      let writeAsset :: (FilePath, BS.ByteString) -> Sem r ()
          writeAsset (filePath, fileContents) =
            Prelude.embed $ BS.writeFile (toAssetsDir </> takeFileName filePath) fileContents
      Prelude.embed (createDirectoryIfMissing True toAssetsDir)
      mapM_ writeAsset assetFiles
      where
        assetFiles :: [(FilePath, BS.ByteString)]
        assetFiles = $(assetsDir)

    tmp :: TopModulePath
    tmp = m ^. modulePath . S.nameConcrete

    srcHtml :: forall s. Members '[Reader HtmlOptions, Embed IO] s => Sem s Html
    srcHtml = do
      utc <- Prelude.embed getCurrentTime
      return (genModuleHtml defaultOptions True utc Ayu m)

    srcHtmlOpts :: HtmlOptions
    srcHtmlOpts = HtmlOptions HtmlSrc

    htmlOpts :: HtmlOptions
    htmlOpts = HtmlOptions HtmlDoc

    docHtml :: forall s. Members '[Reader HtmlOptions] s => Sem s Html
    docHtml = do
      body' <- mbody
      return $
        docTypeHtml $
          mhead
            <> body'
      where
        titleStr :: Html
        titleStr = "Juvix Documentation"

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

        mbody :: Sem s Html
        mbody = do
          content' <- content
          pkgHeader' <- packageHeader
          return $
            body ! Attr.class_ "js-enabled" $
              pkgHeader'
                <> content'
                <> mfooter

        packageHeader :: Sem s Html
        packageHeader = do
          rightMenu' <- rightMenu
          return $
            Html.div ! Attr.id "package-header" $
              ( Html.span ! Attr.class_ "caption" $
                  "package name - version"
              )
                <> rightMenu'
          where
            rightMenu :: Sem s Html
            rightMenu = do
              sourceRef' <- local (set htmlOptionsKind HtmlSrc) (nameIdAttrRef tmp Nothing)
              return $
                ul ! Attr.id "page-menu" ! Attr.class_ "links" $
                  li (a ! Attr.href sourceRef' $ "Source")
                    <> li (a ! Attr.href (preEscapedToValue '#') $ "Index")

        mfooter :: Html
        mfooter = mempty

        content :: Sem s Html
        content = do
          preface' <- docPreface
          interface' <- interface
          return $
            Html.div ! Attr.id "content" $
              moduleHeader
                <> toc
                <> preface'
                <> synopsis
                <> interface'

        synopsis :: Html
        synopsis =
          Html.div ! Attr.id "synopsis" $
            details ! Attr.id "syn" $
              summary "Synopsis"
                <> ( ul
                       ! Attr.class_ "details.toggle"
                       ! dataAttribute "details-id" "syn"
                       $ "Synopsis"
                   )

        docPreface :: Sem s Html
        docPreface = do
          pref <- goJudocMay (m ^. moduleDoc)
          return $
            Html.div ! Attr.id "description" $
              Html.div ! Attr.class_ "doc" $
                pref

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
                li (a ! Attr.href "#description" $ "Description")
                  <> li (a ! Attr.href "#interface" $ "Definitions")

        moduleHeader :: Html
        moduleHeader =
          Html.div ! Attr.id "module-header" $
            (p ! Attr.class_ "caption" $ toHtml (prettyText tmp))

        interface :: Sem s Html
        interface = do
          sigs' <- mconcatMapM goStatement (m ^. moduleBody)
          return $
            Html.div ! Attr.id "interface" $
              Html.h1 "Definitions"
                <> sigs'

goJudocMay :: Members '[Reader HtmlOptions] r => Maybe (Judoc 'Scoped) -> Sem r Html
goJudocMay = maybe (return mempty) goJudoc

goJudoc :: forall r. Members '[Reader HtmlOptions] r => Judoc 'Scoped -> Sem r Html
goJudoc (Judoc atoms) = mconcatMapM goParagraph paragraphs
  where
    goParagraph :: [JudocAtom 'Scoped] -> Sem r Html
    goParagraph = fmap p . mconcatMapM goAtom
    goAtom :: JudocAtom 'Scoped -> Sem r Html
    goAtom = \case
      JudocNewline -> return " "
      JudocExpression e -> ppCodeHtml e
      JudocText txt -> return (toHtml txt)
    paragraphs :: [[JudocAtom 'Scoped]]
    paragraphs = splitOn [JudocNewline, JudocNewline] atoms

goStatement :: Members '[Reader HtmlOptions] r => Statement 'Scoped -> Sem r Html
goStatement = \case
  StatementTypeSignature t -> goTypeSignature t
  StatementAxiom t -> goAxiom t
  StatementInductive t -> goInductive t
  _ -> mempty

goAxiom :: forall r. Members '[Reader HtmlOptions] r => AxiomDef 'Scoped -> Sem r Html
goAxiom axiom = do
  header' <- axiomHeader
  defHeader tmp uid header' Nothing
  where
    uid :: NameId
    uid = axiom ^. axiomName . S.nameId
    tmp :: TopModulePath
    tmp = axiom ^. axiomName . S.nameDefinedIn . S.absTopModulePath
    axiomHeader :: Sem r Html
    axiomHeader = ppCodeHtml axiom

goInductive :: forall r. Members '[Reader HtmlOptions] r => InductiveDef 'Scoped -> Sem r Html
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

goConstructors :: forall r. Members '[Reader HtmlOptions] r => [InductiveConstructorDef 'Scoped] -> Sem r Html
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

defHeader :: forall r. Members '[Reader HtmlOptions] r => TopModulePath -> NameId -> Html -> Maybe (Judoc 'Scoped) -> Sem r Html
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
      return $
        p ! Attr.class_ "src" $
          sig
            <> sourceLink'

goTypeSignature :: forall r. Members '[Reader HtmlOptions] r => TypeSignature 'Scoped -> Sem r Html
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
tagIden x = fromText $ "t:" <> prettyText x

selfLinkName :: IsString a => NameId -> a
selfLinkName x = fromText $ "#" <> tagIden x
