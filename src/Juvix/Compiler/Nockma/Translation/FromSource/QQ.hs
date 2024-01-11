module Juvix.Compiler.Nockma.Translation.FromSource.QQ
  ( module Juvix.Compiler.Nockma.Translation.FromSource.QQ,
    module Juvix.Compiler.Nockma.Language,
  )
where

import Control.Monad.Fail qualified as M
import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Translation.FromSource.Base
import Juvix.Prelude
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

nock :: QuasiQuoter
nock =
  QuasiQuoter
    { quotePat = err,
      quoteDec = err,
      quoteType = err,
      quoteExp = qqNockTerm
    }
  where
    err :: String -> Q a
    err = const (M.fail "QuasiQuote `nock` can only be used as an expression")

    qqNockTerm :: String -> Q Exp
    qqNockTerm = lift . fromMegaParsecError . parseText . pack
