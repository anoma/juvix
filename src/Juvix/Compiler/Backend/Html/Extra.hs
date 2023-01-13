module Juvix.Compiler.Backend.Html.Extra where

import Juvix.Compiler.Backend.Html.Data.Options
import Juvix.Prelude
import Text.Blaze.Html5 as Html hiding (map)
import Text.Blaze.Html5.Attributes qualified as Attr

mathJaxCdn :: Members '[Reader PlainHtmlOptions] r => Sem r Html
mathJaxCdn = do
  baseUrl <- textValue <$> asks (^. htmlOptionsBaseUrl)
  let script1 =
        script
          ! Attr.src "https://polyfill.io/v3/polyfill.min.js?features=es6"
          $ mempty

      script2 =
        script
          ! Attr.type_ "text/javascript"
          ! Attr.id "MathJax-script"
          ! Attr.src (baseUrl <> "assets/tex-chtml.js")
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

linuwialCss :: Members '[Reader PlainHtmlOptions] r => Sem r Html
linuwialCss = do
  baseUrl <- textValue <$> asks (^. htmlOptionsBaseUrl)
  return $
    link
      ! Attr.href (baseUrl <> "assets/linuwial.css")
      ! Attr.rel "stylesheet"
      ! Attr.type_ "text/css"
      ! Attr.title "Linuwial"

sourceCss :: Members '[Reader PlainHtmlOptions] r => Sem r Html
sourceCss = do
  baseUrl <- textValue <$> asks (^. htmlOptionsBaseUrl)
  return $
    link
      ! Attr.href (baseUrl <> "assets/source.css")
      ! Attr.rel "stylesheet"
      ! Attr.type_ "text/css"

cssLink :: Members '[Reader PlainHtmlOptions] r => AttributeValue -> Sem r Html
cssLink css = do
  baseUrl <- textValue <$> asks (^. htmlOptionsBaseUrl)
  return $
    link
      ! Attr.href (baseUrl <> css)
      ! Attr.rel "stylesheet"
      ! Attr.type_ "text/css"

ayuCss :: Members '[Reader PlainHtmlOptions] r => Sem r Html
ayuCss = do
  baseUrl <- textValue <$> asks (^. htmlOptionsBaseUrl)
  cssLink (baseUrl <> "assets/source-ayu-light.css")

nordCss :: Members '[Reader PlainHtmlOptions] r => Sem r Html
nordCss = cssLink "assets/source-nord.css"

highlightJs :: Members '[Reader PlainHtmlOptions] r => Sem r (Html)
highlightJs = do
  baseUrl <- textValue <$> asks (^. htmlOptionsBaseUrl)
  return
    $ script
      ! Attr.src (baseUrl <> "assets/highlight.js")
      ! Attr.type_ "text/javascript"
    $ mempty

metaUtf8 :: Html
metaUtf8 = meta ! Attr.charset "UTF-8"
