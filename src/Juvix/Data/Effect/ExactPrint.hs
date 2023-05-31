module Juvix.Data.Effect.ExactPrint
  ( module Juvix.Data.Effect.ExactPrint,
    module Juvix.Data.Effect.ExactPrint.Base,
  )
where

import Juvix.Data.CodeAnn qualified as C
import Juvix.Data.Effect.ExactPrint.Base
import Juvix.Data.IsImplicit
import Juvix.Prelude.Base hiding ((<+>))
import Juvix.Prelude.Pretty qualified as P

infixr 6 <+>

space :: Members '[ExactPrint] r => Sem r ()
space = noLoc P.space

(<+>) :: Members '[ExactPrint] r => Sem r () -> Sem r () -> Sem r ()
a <+> b = a >> noLoc P.space >> b

infixr 7 ?<>

(?<>) :: Maybe (Sem r ()) -> Sem r () -> Sem r ()
(?<>) = maybe id (<>)

infixr 7 <??+>

(<??+>) :: Members '[ExactPrint] r => Sem r (Maybe ()) -> Sem r () -> Sem r ()
(<??+>) ma b = do
  r <- ma
  case r of
    Nothing -> b
    Just () -> (space <> b)

infixr 7 <?+>

(<?+>) :: Members '[ExactPrint] r => Maybe (Sem r ()) -> Sem r () -> Sem r ()
(<?+>) = \case
  Nothing -> id
  Just a -> (a <+>)

infixl 7 <+?>

(<+?>) :: Members '[ExactPrint] r => Sem r () -> Maybe (Sem r ()) -> Sem r ()
(<+?>) a = maybe a (a <+>)

-- NOTE that then you can use subsume indent' in the call site
-- indent' :: forall ann r a. Sem (ExactPrint ann ': r) a -> Sem (ExactPrint ann ': r) a
-- indent' = region @ann (P.indent 2)

annotated :: Members '[ExactPrint] r => C.CodeAnn -> Sem r () -> Sem r ()
annotated an = region (P.annotate an)

parens :: Members '[ExactPrint] r => Sem r () -> Sem r ()
parens = region C.parens

parensIf :: Members '[ExactPrint] r => Bool -> Sem r () -> Sem r ()
parensIf b
  | b = parens
  | otherwise = id

braces :: Members '[ExactPrint] r => Sem r () -> Sem r ()
braces = region C.braces

lineOrEmpty :: Members '[ExactPrint] r => Sem r ()
lineOrEmpty = noLoc P.line'

grouped :: Members '[ExactPrint] r => Sem r () -> Sem r ()
grouped = region (P.group)

nest :: Members '[ExactPrint] r => Sem r () -> Sem r ()
nest = region (P.nest 2)

hang :: Members '[ExactPrint] r => Sem r () -> Sem r ()
hang = region (P.hang 2)

align :: Members '[ExactPrint] r => Sem r () -> Sem r ()
align = region P.align

indent :: Members '[ExactPrint] r => Sem r () -> Sem r ()
indent = region (P.indent 2)

line :: Members '[ExactPrint] r => Sem r ()
line = noLoc P.line

hardline :: Members '[ExactPrint] r => Sem r ()
hardline = noLoc P.hardline

lbrace :: Members '[ExactPrint] r => Sem r ()
lbrace = noLoc C.kwBraceL

rbrace :: Members '[ExactPrint] r => Sem r ()
rbrace = noLoc C.kwBraceR

bracesIndent :: Members '[ExactPrint] r => Sem r () -> Sem r ()
bracesIndent d = braces (line <> indent d <> line)

semicolon :: Members '[ExactPrint] r => Sem r ()
semicolon = noLoc C.kwSemicolon

blockIndent :: Members '[ExactPrint] r => Sem r () -> Sem r ()
blockIndent d = line <> indent d <> line

endSemicolon :: (Members '[ExactPrint] r, Functor l) => l (Sem r ()) -> l (Sem r ())
endSemicolon = fmap (>> semicolon)

hsep :: (Members '[ExactPrint] r, Foldable l) => l (Sem r ()) -> Sem r ()
hsep = sequenceWith space

vsep :: (Foldable l, Members '[ExactPrint] r) => l (Sem r ()) -> Sem r ()
vsep = sequenceWith line

vsepHard :: (Foldable l, Members '[ExactPrint] r) => l (Sem r ()) -> Sem r ()
vsepHard = sequenceWith hardline

vsep2 :: (Foldable l, Members '[ExactPrint] r) => l (Sem r ()) -> Sem r ()
vsep2 = sequenceWith (line >> line)

enclose :: Monad m => m () -> m () -> m () -> m ()
enclose l r p = l >> p >> r

encloseSep :: (Monad m, Foldable f) => m () -> m () -> m () -> f (m ()) -> m ()
encloseSep l r sep f = l >> sequenceWith sep f >> r

oneLineOrNext :: Members '[ExactPrint] r => Sem r () -> Sem r ()
oneLineOrNext = region P.oneLineOrNext

paragraphs :: (Foldable l, Members '[ExactPrint] r) => l (Sem r ()) -> Sem r ()
paragraphs = sequenceWith (line >> ensureEmptyLine)

delimIf :: Members '[ExactPrint] r => IsImplicit -> Bool -> Sem r () -> Sem r ()
delimIf Implicit _ = braces
delimIf Explicit True = parens
delimIf Explicit False = id
