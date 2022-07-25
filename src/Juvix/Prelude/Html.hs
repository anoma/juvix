module Juvix.Prelude.Html where

import Juvix.Prelude
import Text.Blaze.Html5 as Html hiding (map)
import Text.Blaze.Html5.Attributes qualified as Attr

cssLink :: AttributeValue -> Html
cssLink css =
  link
    ! Attr.href css
    ! Attr.rel "stylesheet"
    ! Attr.type_ "text/css"

ayuCss :: Html
ayuCss = cssLink "assets/source-ayu-light.css"

nordCss :: Html
nordCss = cssLink "assets/source-nord.css"

highlightJs :: Html
highlightJs =
  script
    ! Attr.src "assets/highlight.js"
    ! Attr.type_ "text/javascript"
    $ mempty

metaUtf8 :: Html
metaUtf8 = meta ! Attr.charset "UTF-8"
