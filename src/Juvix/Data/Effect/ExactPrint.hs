module Juvix.Data.Effect.ExactPrint
  ( module Juvix.Data.Effect.ExactPrint,
    module Juvix.Data.Effect.ExactPrint.Base,
  )
where

import Juvix.Data.CodeAnn qualified as C
import Juvix.Data.Effect.ExactPrint.Base
import Juvix.Prelude.Base
import Juvix.Prelude.Pretty qualified as P

infixr 6 <+>

(<+>) :: forall r. (Members '[ExactPrint] r) => Sem r () -> Sem r () -> Sem r ()
a <+> b = a >> noLoc P.space >> b

-- NOTE that then you can use subsume indent' in the call site
-- indent' :: forall ann r a. Sem (ExactPrint ann ': r) a -> Sem (ExactPrint ann ': r) a
-- indent' = region @ann (P.indent 2)

indent :: forall r. Members '[ExactPrint] r => Sem r () -> Sem r ()
indent = region (P.indent 2)

line :: forall r. Members '[ExactPrint] r => Sem r ()
line = noLoc P.line

semicolon :: forall r. Members '[ExactPrint] r => Sem r ()
semicolon = noLoc C.kwSemicolon

sequenceEndWith :: forall l r. Foldable l => Sem r () -> l (Sem r ()) -> Sem r ()
sequenceEndWith sep l = sequenceWith sep l >> sep

endSemicolon :: forall l r. (Members '[ExactPrint] r, Functor l) => l (Sem r ()) -> l (Sem r ())
endSemicolon = fmap (>> semicolon)

sequenceWith :: forall l r. Foldable l => Sem r () -> l (Sem r ()) -> Sem r ()
sequenceWith sep = go . toList
  where
    go :: [Sem r ()] -> Sem r ()
    go = \case
      [] -> return ()
      [x] -> x
      (x : xs) -> x >> sep >> go xs

vsep :: forall r l. (Foldable l, Members '[ExactPrint] r) => l (Sem r ()) -> Sem r ()
vsep = sequenceWith line

vsep2 :: forall l r. (Foldable l, Members '[ExactPrint] r) => l (Sem r ()) -> Sem r ()
vsep2 = sequenceWith (line >> line)
