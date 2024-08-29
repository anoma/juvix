module Juvix.Data.Effect.PartialDo where

import Effectful.Fail qualified as Eff
import Juvix.Data.Effect.Fail
import Juvix.Prelude.Base

type PartialDo = Eff.Fail

runPartialDo :: Sem (PartialDo ': r) a -> Sem r (Either String a)
runPartialDo = Eff.runFail

runPartialDoFail :: (Members '[Fail] r) => Sem (PartialDo ': r) a -> Sem r a
runPartialDoFail m = runPartialDo m >>= either (const fail) return

runPartialDoError :: (Members '[Error String] r) => Sem (PartialDo ': r) a -> Sem r a
runPartialDoError m = runPartialDo m >>= either throw return
