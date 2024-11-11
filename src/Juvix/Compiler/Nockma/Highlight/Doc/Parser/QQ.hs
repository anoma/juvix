module Juvix.Compiler.Nockma.Highlight.Doc.Parser.QQ where

import Control.Monad.Fail qualified as M
import Juvix.Compiler.Nockma.Highlight.Doc.Base
import Juvix.Compiler.Nockma.Highlight.Doc.Parser.Base
import Juvix.Parser.Error (fromMegaParsecError)
import Juvix.Prelude
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

rules :: QuasiQuoter
rules =
  QuasiQuoter
    { quotePat = err,
      quoteDec = err,
      quoteType = err,
      quoteExp = lift . qqRules
    }
  where
    err :: String -> Q a
    err = const (M.fail "QuasiQuote `rules` can only be used as an expression")

    qqRules :: String -> Rules
    qqRules = fromMegaParsecError . parseRules "<quasiquote>" . pack
