module Juvix.Compiler.Backend.Html.Extra where

import Juvix.Compiler.Backend.Html.Data.Options
import Juvix.Prelude
import Text.Blaze.Html5 as Html hiding (map)
import Text.Blaze.Html5.Attributes qualified as Attr

mathJaxCdn :: Members '[Reader HtmlOptions] r => Sem r Html
mathJaxCdn = do
  assetsPrefix <- textValue <$> asks (^. htmlOptionsAssetsPrefix)
  let script1 =
        script
          ! Attr.src "https://polyfill.io/v3/polyfill.min.js?features=es6"
          $ mempty

      script2 =
        script
          ! Attr.type_ "text/javascript"
          ! Attr.id "MathJax-script"
          ! Attr.src (assetsPrefix <> "assets/js/tex-chtml.js")
          $ mempty
  return $ script1 <> script2

-- | This is useful fore debugging only. Note that it only works on a server
-- protocol, opening the file from the local system won't work. For that, one
-- can use @python3 -m http.server@.
livejs :: Html
livejs =
  script
    ! Attr.type_ "text/javascript"
    ! Attr.src "https://livejs.com/live.js"
    $ mempty

cssLink :: Members '[Reader HtmlOptions] r => AttributeValue -> Sem r Html
cssLink css = do
  assetsPrefix <- textValue <$> asks (^. htmlOptionsAssetsPrefix)
  return $
    link
      ! Attr.href (assetsPrefix <> "assets/css/" <> css)
      ! Attr.rel "stylesheet"
      ! Attr.type_ "text/css"

jsLink :: Members '[Reader HtmlOptions] r => AttributeValue -> Sem r Html
jsLink js = do
  assetsPrefix <- textValue <$> asks (^. htmlOptionsAssetsPrefix)
  return
    $ script
      ! Attr.src (assetsPrefix <> "assets/js/" <> js)
      ! Attr.type_ "text/javascript"
    $ mempty

linuwialCss :: Members '[Reader HtmlOptions] r => Sem r Html
linuwialCss = cssLink "linuwial.css"

ayuCss :: Members '[Reader HtmlOptions] r => Sem r Html
ayuCss = cssLink "source-ayu-light.css"

nordCss :: Members '[Reader HtmlOptions] r => Sem r Html
nordCss = cssLink "source-nord.css"

themeCss :: Members '[Reader HtmlOptions] r => Sem r Html
themeCss = do
  theme <- asks (^. htmlOptionsTheme)
  case theme of
    Ayu -> ayuCss
    Nord -> nordCss

highlightJs :: Members '[Reader HtmlOptions] r => Sem r Html
highlightJs = jsLink "highlight.js"

metaUtf8 :: Html
metaUtf8 = meta ! Attr.charset "UTF-8"

taraSmiling :: Members '[Reader HtmlOptions] r => Sem r Html
taraSmiling = do
  assetsPrefix <- textValue <$> asks (^. htmlOptionsAssetsPrefix)
  return $
    Html.img
      ! Attr.id "tara"
      ! Attr.src (assetsPrefix <> "assets/images/tara-smiling.svg")
      ! Attr.alt "Tara"
