module Juvix.Documentation.Compiler where

import Data.ByteString.Builder qualified as Builder
import Juvix.Documentation.Extra
import Juvix.Prelude
import Juvix.Prelude qualified as Prelude
import Juvix.Prelude.Html
import Juvix.Prelude.Pretty
import Juvix.Syntax.Concrete.Language
import Juvix.Syntax.Concrete.Scoped.Name qualified as S
import Juvix.Syntax.Concrete.Scoped.Pretty.Html
import Juvix.Syntax.Concrete.Scoped.Utils
import Juvix.Syntax.NameId
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

moduleDocPath :: Members '[Reader DocParams] r => Module 'Scoped 'ModuleTop -> Sem r FilePath
moduleDocPath m = do
  outDir <- asks (^. docOutputDir)
  return (outDir </> relPath)
  where
  relPath :: FilePath
  relPath = topModulePathToRelativeFilePath (Just "html") joinDot (m ^. modulePath . S.nameConcrete)
  joinDot :: FilePath -> FilePath -> FilePath
  joinDot l r = l <.> r

-- | This function compiles a datalang module into Html documentation.
goTopModule :: Members '[Reader DocParams, Embed IO] r =>
   Module 'Scoped 'ModuleTop -> Sem r ()
goTopModule m = do
  fpath <- moduleDocPath m
  Prelude.embed $ putStrLn ("processing " <> pack fpath)
  writeHtml fpath (run (runReader htmlOpts docHtml))
  where
    htmlOpts :: HtmlOptions
    htmlOpts = HtmlOptions HtmlDoc

    docHtml :: forall s. Members '[Reader HtmlOptions] s => Sem s Html
    docHtml = do
      body' <- mbody
      return $ docTypeHtml $
        mhead
          <> body'
     where
      titleStr :: Html
      titleStr = "Documentation"

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
            <> livejs
            <> ayuCss
            <> linuwialCss

      mbody :: Sem s Html
      mbody = do
        content' <- content
        return $ body ! Attr.class_ "js-enabled" $
          packageHeader
            <> content'
            <> mfooter
      packageHeader :: Html
      packageHeader = mempty
      mfooter :: Html
      mfooter = mempty
      content :: Sem s Html
      content = do
        preface' <- docPreface
        interface' <- interface
        return $ Html.div ! Attr.id "content" $
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
          Html.div ! Attr.class_ "doc" $ pref

      tocEntries :: Html
      tocEntries = mempty
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
      moduleHeader :: Html
      moduleHeader =
        Html.div ! Attr.id "module-header" $
          ( table ! Attr.class_ "info"
            -- \$ tbody (mconcatMap (uncurry metaLine) (HashMap.toList _meta)))
            $
              tbody mempty
          )
            <> (p ! Attr.class_ "caption" $ titleStr)

      interface :: Sem s Html
      interface = do
        sigs' <- mconcatMapM goStatement (m ^. moduleBody)
        return $ Html.div ! Attr.id "interface" $
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
  _ -> mempty

goTypeSignature :: forall r. Members '[Reader HtmlOptions] r => TypeSignature 'Scoped -> Sem r Html
goTypeSignature sig = do
  funHeader' <- functionHeader
  judoc' <- judoc
  return $ Html.div ! Attr.class_ "top" $
    funHeader'
     <> judoc'
  where
    judoc :: Sem r Html
    judoc = do
      judoc' <- goJudocMay (sig ^. sigDoc)
      return (Html.div ! Attr.class_ "doc" $ judoc')

    functionHeader :: Sem r Html
    functionHeader = do
      typeSig' <- typeSig
      return $
        p ! Attr.class_ "src" $
        typeSig'
          <> sourceAndSelfLink (sig ^. sigName . S.nameId)
      where
        typeSig :: Sem r Html
        typeSig = ppCodeHtml (set sigDoc Nothing sig)

sourceAndSelfLink :: NameId -> Html
sourceAndSelfLink name =
  ( a
      ! Attr.href (linkSrcName name)
      ! Attr.class_ "link"
      $ "Source"
  )
    <> ( a
           ! Attr.href (selfLinkName name)
           ! Attr.class_ "selflink"
           $ "#"
       )

linkSrcName :: IsString a => NameId -> a
linkSrcName x = fromText $ (fromString baseName <> "-src.html#") <> prettyText x
  where
    baseName :: String
    baseName = "test"

tagIden :: IsString a => NameId -> a
tagIden x = fromText $ "t:" <> prettyText x

selfLinkName :: IsString a => NameId -> a
selfLinkName x = fromText $ "#" <> tagIden x
