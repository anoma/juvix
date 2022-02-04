module MiniJuvix.Syntax.Concrete.Scoped.Pretty.Html where

import MiniJuvix.Syntax.Concrete.Language
import MiniJuvix.Syntax.Concrete.Scoped.Name (NameKind (..))
import           Prettyprinter.Render.Util.SimpleDocTree
import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Base
import MiniJuvix.Utils.Prelude
import Prettyprinter
import qualified Text.Blaze.Html.Renderer.Text           as Html
import           Text.Blaze.Html5                        as Html
import qualified Text.Blaze.Html5.Attributes             as Attr
import qualified Data.Text as Text
import Data.Text.Lazy (toStrict)


outputTopModuleDefault :: Module 'Scoped 'ModuleTop -> IO ()
outputTopModuleDefault = undefined

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
    STAnn ann content -> putTag ann (go content)
    STConcat l -> mconcatMap go l
    where
    textSpaces :: Int -> Text
    textSpaces n = Text.replicate n (Text.singleton ' ')


putTag :: Ann -> Html -> Html
putTag = undefined
