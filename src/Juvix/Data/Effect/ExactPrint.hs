module Juvix.Data.Effect.ExactPrint
  ( module Juvix.Data.Effect.ExactPrint,
    module Juvix.Data.Effect.ExactPrint.Base,
  )
where

import Juvix.Data.Effect.ExactPrint.Base
import Juvix.Data.CodeAnn qualified as C
import Juvix.Data.CodeAnn (Ann)
import Juvix.Prelude.Base
import Prettyprinter qualified as P

-- NOTE that then you can use subsume indent' in the call site
-- indent' :: forall ann r a. Sem (ExactPrint ann ': r) a -> Sem (ExactPrint ann ': r) a
-- indent' = region @ann (P.indent 2)

indent :: forall ann r a. Members '[ExactPrint ann] r => Proxy ann -> Sem r () -> Sem r ()
indent _ = region @ann (P.indent 2)

line :: forall ann r. Members '[ExactPrint ann] r => Proxy ann -> Sem r ()
line _ = noLoc @ann P.line

semicolon :: forall r. Members '[ExactPrint Ann] r => Sem r ()
semicolon = noLoc @Ann C.kwSemicolon

sequenceEndWith :: forall l ann r. Foldable l => Proxy ann -> Sem r () ->  l (Sem r ()) -> Sem r ()
sequenceEndWith p sep l = sequenceWith p sep l >> sep

endSemicolon :: forall l r. (Members '[ExactPrint Ann] r, Functor l) => l (Sem r ()) -> l (Sem r ())
endSemicolon = fmap (>> semicolon)

sequenceWith :: forall l ann r. Foldable l => Proxy ann -> Sem r () ->  l (Sem r ()) -> Sem r ()
sequenceWith _ sep = go . toList
  where
    go :: [Sem r ()] -> Sem r ()
    go = \case
      [] -> return ()
      [x] -> x
      (x : xs) -> x >> sep >> go xs

vsep :: forall l ann r a. (Foldable l, Members '[ExactPrint ann] r) => Proxy ann -> l (Sem r ()) -> Sem r ()
vsep p = sequenceWith p (line p)

vsep2 :: forall l ann r a. (Foldable l, Members '[ExactPrint ann] r) => Proxy ann -> l (Sem r ()) -> Sem r ()
vsep2 p = sequenceWith p (line p >> line p)
