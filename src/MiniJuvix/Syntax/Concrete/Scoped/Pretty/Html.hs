module MiniJuvix.Syntax.Concrete.Scoped.Pretty.Html (genHtml) where

import MiniJuvix.Syntax.Concrete.Language
import MiniJuvix.Syntax.Concrete.Scoped.Utils
import MiniJuvix.Syntax.Concrete.Scoped.Name (NameKind (..))
import           Prettyprinter.Render.Util.SimpleDocTree
import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Base
import MiniJuvix.Utils.Prelude
import Prettyprinter
import qualified Text.Blaze.Html.Renderer.Text           as Html
import           Text.Blaze.Html5                       as Html hiding (map) 
import qualified Text.Blaze.Html5.Attributes             as Attr
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text.Lazy (toStrict)
import qualified MiniJuvix.Syntax.Concrete.Scoped.Name as S


genHtml :: Options -> Module 'Scoped 'ModuleTop -> IO ()
genHtml opts entry = do
  createDirectoryIfMissing True htmlPath
  withCurrentDirectory htmlPath $ do
    mapM_ outputModule allModules
  where
  allModules = toList $ getAllModules entry
  htmlPath = "html"
  genModule :: Module 'Scoped 'ModuleTop -> Text
  genModule = renderHtml . docStream opts
  outputModule :: Module 'Scoped 'ModuleTop -> IO ()
  outputModule m = do
    createDirectoryIfMissing True (takeDirectory htmlFile)
    putStrLn $ "Writing " <> htmlFile
    Text.writeFile htmlFile (genModule m)
   where
   htmlFile = dottedPath (S._nameConcrete (modulePath m)) <.> ".html"

renderHtml :: SimpleDocStream Ann -> Text
renderHtml = toStrict . Html.renderHtml . renderTree . treeForm

docStream :: Options -> Module 'Scoped 'ModuleTop -> SimpleDocStream Ann
docStream opts m = layoutPretty defaultLayoutOptions (prettyTopModule opts m)

renderTree :: SimpleDocTree Ann -> Html
renderTree sdt =
    docType <> (Html.body ! sty1 $ pre ! sty2 $ go sdt)
  where
  sty1 = Attr.style ("background: #fdf6e3; overflow:auto; width:auto; font-size:"
                      <> defFontSize <> ";padding: 3em 15% 5em 15%")
  sty2 = Attr.style "margin: 0; line-height: 125%"
  defFontSize :: AttributeValue
  defFontSize = "14pt"


go :: SimpleDocTree Ann -> Html
go sdt = case sdt of
    STEmpty -> mempty
    STChar c -> toHtml c
    STText _ t -> toHtml t
    STLine s -> "\n" <> toHtml (textSpaces s)
    STAnn ann content -> putTag defaultTheme ann (go content)
    STConcat l -> mconcatMap go l
    where
    textSpaces :: Int -> Text
    textSpaces n = Text.replicate n (Text.singleton ' ')

fromText :: IsString a => Text -> a
fromText = fromString . unpack

color :: Color -> Attribute
color c = Attr.style (fromText $ "color:" <> c)

putTag :: Theme -> Ann -> Html -> Html
putTag Theme {..} ann x = case ann of
   AnnKind {} -> x
   AnnKeyword -> strong x ! color _kwColor
   AnnDelimiter -> x
   AnnDef ni -> u $ Html.a ! Attr.name (nameIdAttr ni)
                           ! color _typeNameColor
                             $ x
   AnnRef tmp ni -> a ! Attr.href (nameIdAttrRef tmp ni)
                      ! color _typeRefColor
                      $ x
dottedPath :: IsString s => TopModulePath -> s
dottedPath (TopModulePath l r) =
  fromText $ mconcat $ intersperse "." $ map fromSymbol $ l ++ [r]
  where
  fromSymbol (Sym t) = t

nameIdAttr :: S.NameId -> AttributeValue
nameIdAttr (S.NameId k) = fromString . show $ k

nameIdAttrRef :: TopModulePath -> S.NameId -> AttributeValue
nameIdAttrRef tp s =
  dottedPath tp <> ".html" <> preEscapedToValue '#' <> nameIdAttr s

type Color = Text

data Theme = Theme {
  _kwColor :: Color,
  _symColor :: Color,
  _fieldColor :: Color,
  _typeRefColor :: Color,
  _typeNameColor :: Color
  }

defaultTheme :: Theme
defaultTheme = Theme {
  _kwColor = "#c43063",
  _symColor = "#df443f",
  _fieldColor = "#388057",
  _typeRefColor = "#de7185",
  _typeNameColor = "#e98176"
  }
