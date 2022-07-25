module Juvix.Documentation.Compiler where

import Data.Text.Lazy qualified as Text
import Juvix.Documentation.Extra
import Juvix.Prelude
import Juvix.Prelude.Html
import Juvix.Prelude.Pretty
import Juvix.Syntax.Concrete.Language
import Juvix.Syntax.Concrete.Scoped.Name qualified as S
import Juvix.Syntax.Concrete.Scoped.Pretty.Html
import Juvix.Syntax.NameId
-- import Data.HashMap.Strict    qualified          as HashMap
import Text.Blaze.Html.Renderer.Text qualified as Html
import Text.Blaze.Html5 as Html hiding (map)
import Text.Blaze.Html5.Attributes qualified as Attr

compileModuleHtmlText :: String -> Module 'Scoped 'ModuleTop -> Text
compileModuleHtmlText baseName = Text.toStrict . Html.renderHtml . compileModule baseName

-- | This function compiles a datalang module into Html documentation.
compileModule ::
  -- | Base name
  String ->
  Module 'Scoped 'ModuleTop ->
  Html
compileModule _ m' =
  docTypeHtml $
    mhead
      <> mbody
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

    mbody :: Html
    mbody =
      body ! Attr.class_ "js-enabled" $
        packageHeader
          <> content
          <> mfooter
    packageHeader :: Html
    packageHeader = mempty
    mfooter :: Html
    mfooter = mempty
    content :: Html
    content =
      Html.div ! Attr.id "content" $
        moduleHeader
          <> toc
          <> docPreface
          <> synopsis
          <> interface
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
    docPreface :: Html
    docPreface =
      Html.div ! Attr.id "description" $
        Html.div ! Attr.class_ "doc" $
          goJudocMay (m' ^. moduleDoc)
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
    -- where
    -- metaLine :: Text -> Text -> Html
    -- metaLine k v = tr (th (toHtml k) <> td (toHtml v))
    interface :: Html
    interface =
      Html.div ! Attr.id "interface" $
        Html.h1 "Definitions"
          <> mconcatMap goStatement (m' ^. moduleBody)

-- <> mconcatMap compileFunDef _functionDefs

goJudocMay :: Maybe (Judoc 'Scoped) -> Html
goJudocMay = maybe mempty goJudoc

goJudoc :: Judoc 'Scoped -> Html
goJudoc (Judoc atoms) = mconcatMap goParagraph paragraphs
  where
    goParagraph :: [JudocAtom 'Scoped] -> Html
    goParagraph = p . mconcatMap goAtom
    goAtom :: JudocAtom 'Scoped -> Html
    goAtom = \case
      JudocNewline -> " "
      JudocExpression e -> ppCodeHtml e
      JudocText txt -> toHtml txt
    paragraphs :: [[JudocAtom 'Scoped]]
    paragraphs = splitOn [JudocNewline, JudocNewline] atoms

-- compileBlocks :: [Block] -> Html
-- compileBlocks = compilePandoc . Pandoc mempty

-- compileInlines :: [Inline] -> Html
-- compileInlines = compilePandoc . Pandoc mempty . pure . Para

-- labelHeaders :: ModuleDef -> ModuleDef
-- labelHeaders m =
--   m & documentation . L.body %~ aux
--   where
--   aux :: [Block] -> [Block]
--   aux b = flattenForest (evalState (go $ forest b) 1)
--   forest :: [Block] -> PandocForest
--   forest b = fromMaybe (error "labelHeaders") $ parsePandocForest b
--   go :: PandocForest -> State Int PandocForest
--   go (PandocForest _preface _sections)  = PandocForest _preface <$> mapM goh _sections
--   goh :: (HeaderNode, PandocForest) -> State Int (HeaderNode, PandocForest)
--   goh (hn, f) = do
--    i <- get
--    let hn' = hn & inlines %~ addSpan i
--    modify (+1)
--    f' <- go f
--    return (hn', f')
--   addSpan :: Int -> [Inline] -> [Inline]
--   addSpan i l = [Span (headerIdPrefix <> showT i, mempty, mempty) l]

-- headerIdPrefix :: Text
-- headerIdPrefix = "g:"

-- compilePandocBlock :: Pandoc -> Html
-- compilePandocBlock d =
--  fromRight (error "compileBlock: pandoc error")
--                    $ runPure (writeHtml5 def d)

-- headerIden :: Text -> Html
-- headerIden funName =
--   a ! Attr.id (tagIden funName)
--     ! Attr.class_ "def"
--     $ toHtml funName

-- compileFunArgIdentifierDef :: Text -> Text -> Html
-- compileFunArgIdentifierDef fun arg =
--   a ! Attr.id (fromText $ tagIden (qualifyFunArg fun arg))
--     $ toHtml arg

-- compileFunDef :: TypeSignature 'Scoped -> Html
-- compileFunDef f@TypeSignature{..} = Html.div ! Attr.class_ "top" $
--   functionHeader
--   <> subsArguments
--   <> docPart _funDefDoc
--  where
-- hasSignature :: Bool
-- hasSignature = not (any hasDoc _funArgs)
--   where hasDoc :: FunArg -> Bool
--         hasDoc = (/=) mempty . _argDoc
-- subsArguments :: Html
-- subsArguments
--  | hasSignature = mempty
--  | otherwise = p ! Attr.class_ "subs arguments" $
--    table $ tbody $ compileArgs _funArgs <> compileRet _funRet
--    where
--    compileArgs :: [FunArg] -> Html
--    compileArgs = mconcatMap compileArgLine . zip (":" : repeat "→")
--    compileArgLine :: (Html, FunArg) -> Html
--    compileArgLine (prefix, arg@FunArg{..}) =
--      tr $
--         (td ! Attr.class_ "src" $ prefix
--          <> compileArg _funName arg) <> (td ! Attr.class_ "doc" $ compileBlocks _argDoc)
--    compileRet :: Type -> Html
--    compileRet ret =
--      tr $
--         (td ! Attr.class_ "src" $ "→ " <> compileType ret)
--      <> (td ! Attr.class_ "doc empty" $ mempty)

--  This method is used when none of the arguments has specific documentation.
-- functionHeader :: Html
-- functionHeader =
--  p ! Attr.class_ "src" $
--    (Html.span ! aKeyword) "fun "
--      <> headerIden _funName
--      <> typeSig
--      <> sourceAndSelfLink _funName
--   where
--   typeSig :: Html
--   typeSig
--     | hasSignature = " : " <> mconcat (intersperse " → " (map (compileArg _funName) _funArgs <> [compileType _funRet]))
--     | otherwise = mempty

-- compileArg :: Text -> FunArg -> Html
-- compileArg funName FunArg{..} = case _argName of
--   Just arg -> " (" <> compileFunArgIdentifierDef funName arg <> " : " <> compileType _argType <> ")"
--   Nothing -> compileType _argType

-- docPart :: [Block] -> Html
-- docPart pand = Html.div ! Attr.class_ "doc"
--     $ compileBlocks pand

-- compileDataDef :: DataDef -> Html
-- compileDataDef d@DataDef{..} = Html.div ! Attr.class_ "top" $
--   compileDataDefHeader d
--   <> docPart _defDoc
--   <> rhsPart
--   where
--   rhsPart = compileDataDefRhs _rhs

-- compileDataDefHeader :: DataDef -> Html
-- compileDataDefHeader DataDef{..} =
--   p ! Attr.class_ "src" $
--     (Html.span ! aKeyword) "type "
--       <> headerIden _name
--       <> maybeSynonym
--       <> sourceAndSelfLink _name
--   where
--     maybeSynonym = case _rhs of
--       Synonym{..} -> " = " <> compileLeafType _synonymType
--       _           -> mempty

-- compileDataDefRhs :: DataDefRhs -> Html
-- compileDataDefRhs d = case d of
--   Record{..} -> compileFields _fields
--   SumType{..} -> Html.div ! Attr.class_ "subs constructors"
--     $ (p ! Attr.class_ "caption" $ "Constructors")
--      <> compileConstructors _constructors
--   Synonym{..} -> mempty

-- compileConstructor :: ConstructorDef -> Html
-- compileConstructor ConstructorDef{..} =
--   tr (srcPart <> docPart)
--   <> compileConstructorArg _constructorArg
--   where
--     docPart = td ! Attr.class_ "doc"
--                  $ compileBlocks _constructorDoc
--     srcPart = td ! Attr.class_ "src" $
--       a ! Attr.id (constructorAttrId _constructorName)
--         ! Attr.class_ "def"
--         $ toHtml _constructorName

-- compileConstructorArg :: ConstructorArg -> Html
-- compileConstructorArg a = case a of
--   None      -> mempty
--   Of l      -> mempty
--   Fields fs -> compileFields fs

-- compileFields :: [FieldDef] -> Html
-- compileFields fs = tr
--   $ td ! Attr.colspan "2"
--   $ Html.div ! Attr.class_ "subs fields"
--   $ ul (mconcatMap compileField fs)

-- compileField :: FieldDef -> Html
-- compileField FieldDef{..} = li $ srcPart <> docPart
--   where
--     docPart = compileBlocks _fieldDoc
--     srcPart = Html.dfn ! Attr.class_ "src"
--      $ (a ! Attr.id (fieldAttrId _fieldName)
--           ! Attr.class_ "def"
--           $ toHtml _fieldName)
--      <> " :: "
--      <> compileType _fieldType

-- compileType :: Type -> Html
-- compileType ty = case ty of
--   Leaf l   -> compileLeafType l
--   ListOf l -> "list of " <> compileLeafType l

-- compileLeafType :: LeafType -> Html
-- compileLeafType ty = case ty of
--   TyDataDef tyName -> a ! Attr.href (selfLinkName tyName)
--                         $ toHtml tyName
--   TyInteger -> "int"
--   TyString -> "string"
--   TyFloat -> "float"
--   TyBool -> "bool"

-- compileConstructors :: [ConstructorDef] -> Html
-- compileConstructors = table . tbody . mconcatMap compileConstructor

goStatement :: Statement 'Scoped -> Html
goStatement = \case
  StatementTypeSignature t -> goTypeSignature t
  _ -> mempty

goTypeSignature :: TypeSignature 'Scoped -> Html
goTypeSignature sig =
  Html.div ! Attr.class_ "top" $
    functionHeader
      <> judoc
  where
    judoc :: Html
    judoc =
      Html.div ! Attr.class_ "doc" $
        maybe mempty goJudoc (sig ^. sigDoc)

    functionHeader :: Html
    functionHeader =
      p ! Attr.class_ "src" $
        typeSig
          <> sourceAndSelfLink (sig ^. sigName . S.nameId)
      where
        typeSig :: Html
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
