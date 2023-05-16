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
import Juvix.Compiler.Abstract.Translation.FromConcrete qualified as Abstract
import Juvix.Compiler.Backend.Html.Data
import Juvix.Compiler.Backend.Html.Extra
import Juvix.Compiler.Backend.Html.Translation.FromTyped.Source hiding (go)
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Extra
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Print
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoped
import Juvix.Compiler.Internal.Translation.FromAbstract qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Data.Context qualified as InternalArity
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking qualified as InternalTyped
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Extra.Assets
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude
import Juvix.Prelude qualified as Prelude
import Juvix.Prelude.Pretty
import Text.Blaze.Html.Renderer.Utf8 qualified as Html
import Text.Blaze.Html5 as Html hiding (map)
import Text.Blaze.Html5.Attributes qualified as Attr

data JudocArgs = JudocArgs
  { _judocArgsOutputDir :: Path Abs Dir,
    _judocArgsBaseName :: Text,
    _judocArgsAssetsPrefix :: Text,
    _judocArgsUrlPrefix :: Text,
    _judocArgsCtx :: InternalTypedResult,
    _judocArgsTheme :: Theme,
    _judocArgsNonRecursive :: Bool,
    _judocArgsNoFooter :: Bool
  }

makeLenses ''JudocArgs

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

indexFileName :: Path Rel File
indexFileName = $(mkRelFile "index.html")

createIndexFile ::
  forall r.
  (Members '[Embed IO, Reader HtmlOptions, Reader EntryPoint] r) =>
  [TopModulePath] ->
  Sem r ()
createIndexFile ps = do
  outputDir <- asks (^. htmlOptionsOutputDir)
  indexHtml >>= (template mempty >=> writeHtml (outputDir <//> indexFileName))
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
              Html.p ! Attr.class_ attrBare $
                toHtml (prettyText s)
          Just lbl' -> do
            lnk <- nameIdAttrRef lbl' Nothing
            return $
              Html.span ! Attr.class_ attrBare $
                (a ! Attr.href lnk $ toHtml (prettyText lbl'))

        attrBase :: Html.AttributeValue
        attrBase = "details-toggle-control details-toggle"

        attrBare :: Html.AttributeValue
        attrBare = attrBase <> " directory"

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
                        summary "Subtree"
                          <> ul (mconcatMap li c')

writeHtml :: (Members '[Embed IO] r) => Path Abs File -> Html -> Sem r ()
writeHtml f h = Prelude.embed $ do
  ensureDir dir
  Builder.writeFile (toFilePath f) (Html.renderHtmlBuilder h)
  where
    dir :: Path Abs Dir
    dir = parent f

genJudocHtml :: (Members '[Embed IO] r) => JudocArgs -> Sem r ()
genJudocHtml JudocArgs {..} =
  runReader htmlOpts . runReader normTable . runReader entry $ do
    Prelude.embed (writeAssets _judocArgsOutputDir)
    mapM_ (goTopModule cs) allModules
    createIndexFile (map topModulePath (toList allModules))
  where
    cs :: Comments
    cs =
      _judocArgsCtx
        ^. resultInternalArityResult
          . InternalArity.resultInternalResult
          . Internal.resultAbstract
          . Abstract.resultScoper
          . Scoped.comments

    entry :: EntryPoint
    entry = _judocArgsCtx ^. InternalTyped.internalTypedResultEntryPoint

    normTable :: InternalTyped.NormalizedTable
    normTable = _judocArgsCtx ^. InternalTyped.resultNormalized

    mainMod :: Module 'Scoped 'ModuleTop
    mainMod =
      _judocArgsCtx
        ^. InternalTyped.resultInternalArityResult
          . InternalArity.resultInternalResult
          . Internal.resultAbstract
          . Abstract.resultScoper
          . Scoped.mainModule

    htmlOpts :: HtmlOptions
    htmlOpts =
      HtmlOptions
        { _htmlOptionsKind = HtmlDoc,
          _htmlOptionsAssetsPrefix = _judocArgsAssetsPrefix,
          _htmlOptionsOutputDir = _judocArgsOutputDir,
          _htmlOptionsUrlPrefix = _judocArgsUrlPrefix,
          _htmlOptionsParamBase = _judocArgsBaseName,
          _htmlOptionsTheme = _judocArgsTheme,
          _htmlOptionsNoFooter = _judocArgsNoFooter
        }

    allModules
      | _judocArgsNonRecursive = pure mainMod
      | otherwise = toList topModules

    topModules :: HashMap NameId (Module 'Scoped 'ModuleTop)
    topModules = getAllModules mainMod

moduleDocPath :: (Members '[Reader HtmlOptions] r) => Module 'Scoped 'ModuleTop -> Sem r (Path Abs File)
moduleDocPath m = do
  relPath <- moduleDocRelativePath (m ^. modulePath . S.nameConcrete)
  outputDir <- asks (^. htmlOptionsOutputDir)
  return (outputDir <//> relPath)

topModulePath :: Module 'Scoped 'ModuleTop -> TopModulePath
topModulePath = (^. modulePath . S.nameConcrete)

template :: forall r. (Members '[Reader EntryPoint, Reader HtmlOptions] r) => Html -> Html -> Sem r Html
template rightMenu' content' = do
  mathJax <- mathJaxCdn
  ayuTheme <- ayuCss
  judocTheme <- linuwialCss
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

      titleStr :: Html
      titleStr = "Juvix Documentation"

      packageHeader :: Sem r Html
      packageHeader = do
        pkgName' <- toHtml <$> asks (^. entryPointPackage . packageName)
        version' <- toHtml <$> asks (^. entryPointPackage . packageVersion . to prettySemVer)
        return $
          Html.div ! Attr.id "package-header" $
            ( Html.span ! Attr.class_ "caption" $
                pkgName' <> " - " <> version'
            )
              <> rightMenu'

      mbody :: Sem r Html
      mbody = do
        bodyHeader' <- packageHeader
        footer' <- htmlJuvixFooter
        return $
          body ! Attr.class_ "js-enabled" $
            bodyHeader'
              <> content'
              <> footer'

  body' <- mbody
  return $ docTypeHtml (mhead <> body')

-- | This function compiles a module into Html documentation.
goTopModule ::
  forall r.
  (Members '[Reader HtmlOptions, Embed IO, Reader EntryPoint, Reader NormalizedTable] r) =>
  Comments ->
  Module 'Scoped 'ModuleTop ->
  Sem r ()
goTopModule cs m = do
  htmlOpts <- ask @HtmlOptions
  runReader (htmlOpts {_htmlOptionsKind = HtmlDoc}) $ do
    fpath <- moduleDocPath m
    Prelude.embed (putStrLn ("Writing " <> pack (toFilePath fpath)))
    docHtml >>= writeHtml fpath

  runReader (htmlOpts {_htmlOptionsKind = HtmlSrc}) $ do
    fpath <- moduleDocPath m
    srcHtml >>= writeHtml fpath
  where
    tmp :: TopModulePath
    tmp = m ^. modulePath . S.nameConcrete

    srcHtml :: forall s. (Members '[Reader HtmlOptions, Embed IO] s) => Sem s Html
    srcHtml = do
      utc <- Prelude.embed getCurrentTime
      genModuleHtml
        GenModuleHtmlArgs
          { _genModuleHtmlArgsConcreteOpts = defaultOptions,
            _genModuleHtmlArgsUTC = utc,
            _genModuleHtmlArgsComments = cs,
            _genModuleHtmlArgsModule = m
          }

    docHtml :: forall s. (Members '[Reader HtmlOptions, Reader EntryPoint, Reader NormalizedTable] s) => Sem s Html
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
              li (a ! Attr.href sourceRef' $ "Source") -- TODO: review here
                <> li (a ! Attr.href (fromString (toFilePath indexFileName)) $ "Index")

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

goJudocMay :: (Members '[Reader HtmlOptions, Reader NormalizedTable] r) => Maybe (Judoc 'Scoped) -> Sem r Html
goJudocMay = maybe (return mempty) goJudoc

goJudoc :: forall r. (Members '[Reader HtmlOptions, Reader NormalizedTable] r) => Judoc 'Scoped -> Sem r Html
goJudoc (Judoc bs) = mconcatMapM goBlock bs
  where
    goBlock :: JudocBlock 'Scoped -> Sem r Html
    goBlock = \case
      JudocParagraphLines ls -> Html.p . concatWith (\l r -> l <> " " <> r) <$> mapM goLine (toList ls)
      JudocParagraphBlock {} -> undefined
      JudocExample e -> goExample e

    goLine :: JudocParagraphLine 'Scoped -> Sem r Html
    goLine (JudocParagraphLine atoms) = mconcatMapM goAtom (fmap (^. withLocParam) atoms)

    goExample :: Example 'Scoped -> Sem r Html
    goExample ex = do
      e' <- ppCodeHtml defaultOptions (ex ^. exampleExpression)
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
      JudocExpression e -> ppCodeHtml defaultOptions e
      JudocText txt -> return (toHtml txt)

goStatement :: (Members '[Reader HtmlOptions, Reader NormalizedTable] r) => Statement 'Scoped -> Sem r Html
goStatement = \case
  StatementTypeSignature t -> goTypeSignature t
  StatementAxiom t -> goAxiom t
  StatementInductive t -> goInductive t
  StatementOpenModule t -> goOpen t
  _ -> mempty

goOpen :: forall r. (Members '[Reader HtmlOptions] r) => OpenModule 'Scoped -> Sem r Html
goOpen op
  | Public <- op ^. openPublic = noDefHeader <$> ppCodeHtml defaultOptions op
  | otherwise = mempty

goAxiom :: forall r. (Members '[Reader HtmlOptions, Reader NormalizedTable] r) => AxiomDef 'Scoped -> Sem r Html
goAxiom axiom = do
  header' <- axiomHeader
  defHeader tmp uid header' (axiom ^. axiomDoc)
  where
    uid :: NameId
    uid = axiom ^. axiomName . S.nameId
    tmp :: TopModulePath
    tmp = axiom ^. axiomName . S.nameDefinedIn . S.absTopModulePath
    axiomHeader :: Sem r Html
    axiomHeader = ppCodeHtml defaultOptions (set axiomDoc Nothing axiom)

goInductive :: forall r. (Members '[Reader HtmlOptions, Reader NormalizedTable] r) => InductiveDef 'Scoped -> Sem r Html
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
    inductiveHeader = do
      docToHtml (run (runReader defaultOptions (execExactPrint Nothing (ppInductiveSignature def))))

goConstructors :: forall r. (Members '[Reader HtmlOptions, Reader NormalizedTable] r) => NonEmpty (InductiveConstructorDef 'Scoped) -> Sem r Html
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
          sig' <- ppCodeHtml defaultOptions (set constructorDoc Nothing c)
          return $
            td ! Attr.class_ "src" $
              sig'

noDefHeader :: Html -> Html
noDefHeader = p ! Attr.class_ "src"

defHeader :: forall r. (Members '[Reader HtmlOptions, Reader NormalizedTable] r) => TopModulePath -> NameId -> Html -> Maybe (Judoc 'Scoped) -> Sem r Html
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

goTypeSignature :: forall r. (Members '[Reader HtmlOptions, Reader NormalizedTable] r) => TypeSignature 'Scoped -> Sem r Html
goTypeSignature sig = do
  sig' <- typeSig
  defHeader tmp uid sig' (sig ^. sigDoc)
  where
    tmp :: TopModulePath
    tmp = sig ^. sigName . S.nameDefinedIn . S.absTopModulePath
    uid :: NameId
    uid = sig ^. sigName . S.nameId
    typeSig :: Sem r Html
    typeSig = ppCodeHtml defaultOptions (set sigDoc Nothing sig)

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

tagIden :: (IsString a) => NameId -> a
tagIden x = fromText $ prettyText x

selfLinkName :: (IsString a) => NameId -> a
selfLinkName x = fromText $ "#" <> tagIden x
