module Juvix.Documentation.Extra where

import Juvix.Prelude
import Text.Blaze.Html5 as Html hiding (map)
import Text.Blaze.Html5.Attributes qualified as Attr

mathJaxCdn :: Html
mathJaxCdn = script1 <> script2
  where
    script1 =
      script
        ! Attr.src src1
        $ mempty
    script2 =
      script
        ! Attr.type_ "text/javascript"
        ! Attr.id "MathJax-script"
        ! Attr.src src2
        $ mempty
    src1 :: AttributeValue
    src1 = "https://polyfill.io/v3/polyfill.min.js?features=es6"
    src2 :: AttributeValue
    src2 = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js"

-- | This is useful fore debugging only. Note that it only works on a server
-- protocol, opening the file from the local system won't work. For that, one
-- can use @python3 -m http.server@.
livejs :: Html
livejs =
  script
    ! Attr.type_ "text/javascript"
    ! Attr.src "https://livejs.com/live.js"
    $ mempty

linuwialCss :: Html
linuwialCss =
  link
    ! Attr.href "assets/linuwial.css"
    ! Attr.rel "stylesheet"
    ! Attr.type_ "text/css"
    ! Attr.title "Linuwial"

sourceCss :: Html
sourceCss =
  link
    ! Attr.href "assets/source.css"
    ! Attr.rel "stylesheet"
    ! Attr.type_ "text/css"

highlightJs :: Html
highlightJs =
  script
    ! Attr.src "assets/highlight.js"
    ! Attr.type_ "text/javascript"
    $ mempty
