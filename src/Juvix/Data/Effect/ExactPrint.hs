module Juvix.Data.Effect.ExactPrint
  ( module Juvix.Data.Effect.ExactPrint,
    module Juvix.Data.Effect.ExactPrint.Base,
  )
where

import Juvix.Data.CodeAnn qualified as C
import Juvix.Data.Effect.ExactPrint.Base
import Juvix.Data.IsImplicit
import Juvix.Data.Keyword.All
import Juvix.Data.WithLoc
import Juvix.Prelude.Base hiding ((<+>))
import Juvix.Prelude.Pretty qualified as P

infixr 6 <+>

space :: (Members '[ExactPrint] r) => Sem r ()
space = noLoc P.space

(<+>) :: (Members '[ExactPrint] r) => Sem r () -> Sem r () -> Sem r ()
a <+> b = a >> noLoc P.space >> b

infixr 7 ?<>

(?<>) :: Maybe (Sem r ()) -> Sem r () -> Sem r ()
(?<>) = maybe id (<>)

infixr 7 <??+>

(<??+>) :: (Members '[ExactPrint] r) => Sem r (Maybe ()) -> Sem r () -> Sem r ()
(<??+>) ma b = do
  r <- ma
  case r of
    Nothing -> b
    Just () -> (space <> b)

infixr 7 <?+>

(<?+>) :: (Members '[ExactPrint] r) => Maybe (Sem r ()) -> Sem r () -> Sem r ()
(<?+>) = \case
  Nothing -> id
  Just a -> (a <+>)

infixl 7 <+?>

(<+?>) :: (Members '[ExactPrint] r) => Sem r () -> Maybe (Sem r ()) -> Sem r ()
(<+?>) a = maybe a (a <+>)

annotated :: (Members '[ExactPrint] r) => C.CodeAnn -> Sem r () -> Sem r ()
annotated an = region (P.annotate an)

-- | Opening parenthesis is printed after comments
parens :: (Members '[ExactPrint] r) => Sem r () -> Sem r ()
parens = enclose (enqueue C.kwParenL) (noLoc C.kwParenR)

parensIf :: (Members '[ExactPrint] r) => Bool -> Sem r () -> Sem r ()
parensIf b
  | b = parens
  | otherwise = id

-- | Opening brace is printed after comments
braces :: (Members '[ExactPrint] r) => Sem r () -> Sem r ()
braces = enclose (enqueue C.kwBraceL) (noLoc C.kwBraceR)

lineOrEmpty :: (Members '[ExactPrint] r) => Sem r ()
lineOrEmpty = noLoc P.line'

grouped :: (Members '[ExactPrint] r) => Sem r () -> Sem r ()
grouped = region P.group

nest :: (Members '[ExactPrint] r) => Sem r () -> Sem r ()
nest = region (P.nest 2)

hang :: (Members '[ExactPrint] r) => Sem r () -> Sem r ()
hang = region (P.hang 2)

align :: (Members '[ExactPrint] r) => Sem r () -> Sem r ()
align = region P.align

indent :: (Members '[ExactPrint] r) => Sem r () -> Sem r ()
indent = region (P.indent 2)

line :: (Members '[ExactPrint] r) => Sem r ()
line = noLoc P.line

hardline :: (Members '[ExactPrint] r) => Sem r ()
hardline = noLoc P.hardline

colon :: (Members '[ExactPrint] r) => Sem r ()
colon = noLoc C.kwColon

semicolon :: (Members '[ExactPrint] r) => Sem r ()
semicolon = noLoc C.kwSemicolon

blockIndent :: (Members '[ExactPrint] r) => Sem r () -> Sem r ()
blockIndent d = hardline <> indent d <> line

sep :: (Members '[ExactPrint] r, Foldable l) => l (Sem r ()) -> Sem r ()
sep = grouped . vsep

sepSemicolon :: (Members '[ExactPrint] r, Foldable l) => l (Sem r ()) -> Sem r ()
sepSemicolon = grouped . vsepSemicolon

vsepSemicolon :: (Members '[ExactPrint] r, Foldable l) => l (Sem r ()) -> Sem r ()
vsepSemicolon = sequenceWith (semicolon <> line)

hsepSemicolon :: (Members '[ExactPrint] r, Foldable l) => l (Sem r ()) -> Sem r ()
hsepSemicolon = sequenceWith (semicolon <> space)

dotted :: (Foldable f, Members '[ExactPrint] r) => f (Sem r ()) -> Sem r ()
dotted = sequenceWith (noLoc C.kwDot)

sepEndSemicolon :: (Members '[ExactPrint] r, Functor l) => l (Sem r ()) -> l (Sem r ())
sepEndSemicolon = fmap (>> semicolon)

hsep :: (Members '[ExactPrint] r, Foldable l) => l (Sem r ()) -> Sem r ()
hsep = sequenceWith space

vsep :: (Foldable l, Members '[ExactPrint] r) => l (Sem r ()) -> Sem r ()
vsep = sequenceWith line

vsepHard :: (Foldable l, Members '[ExactPrint] r) => l (Sem r ()) -> Sem r ()
vsepHard = sequenceWith hardline

vsep2 :: (Foldable l, Members '[ExactPrint] r) => l (Sem r ()) -> Sem r ()
vsep2 = sequenceWith (line >> line)

enclose1 :: (Monad m) => m () -> m () -> m ()
enclose1 lr = enclose lr lr

surround :: (Monad m) => m () -> m () -> m () -> m ()
surround x l r = enclose l r x

enclose :: (Monad m) => m () -> m () -> m () -> m ()
enclose l r p = l >> p >> r

encloseSep :: (Monad m, Foldable f) => m () -> m () -> m () -> f (m ()) -> m ()
encloseSep l r separator f = enclose l r (sequenceWith separator f)

oneLineOrNextNoIndent :: (Members '[ExactPrint] r) => Sem r () -> Sem r ()
oneLineOrNextNoIndent = region P.oneLineOrNextNoIndent

oneLineOrNext :: (Members '[ExactPrint] r) => Sem r () -> Sem r ()
oneLineOrNext = region P.oneLineOrNext

nextLine :: (Members '[ExactPrint] r) => Sem r () -> Sem r ()
nextLine = region P.nextLine

paragraphs :: (Foldable l, Members '[ExactPrint] r) => l (Sem r ()) -> Sem r ()
paragraphs = sequenceWith (line >> ensureEmptyLine)

kw :: (Members '[ExactPrint] r) => Keyword -> Sem r ()
kw = annotated C.AnnKeyword . noLoc . P.pretty

keywordText :: (Members '[ExactPrint] r) => Text -> Sem r ()
keywordText = annotated C.AnnKeyword . noLoc . P.pretty

-- | The first argument contains the left and right delimiters, if any.
-- If the second argument is True, then the delimiters *must* be given.
delimIf' :: Maybe (Sem r (), Sem r ()) -> IsImplicit -> Bool -> Sem r () -> Sem r ()
delimIf' d impl delim
  | delim || impl == Implicit = uncurry enclose (fromJust d)
  | otherwise = id

delimIf :: (Members '[ExactPrint] r) => IsImplicit -> Bool -> Sem r () -> Sem r ()
delimIf Implicit _ = braces
delimIf Explicit True = parens
delimIf Explicit False = id

morphemeWithLoc :: forall r. (Members '[ExactPrint] r) => WithLoc (Doc C.CodeAnn) -> Sem r ()
morphemeWithLoc (WithLoc loc doc) = morpheme loc doc

morpheme :: forall r. (Members '[ExactPrint] r) => Interval -> Doc C.CodeAnn -> Sem r ()
morpheme loc doc = do
  void (printCommentsUntil loc)
  noLoc doc

morphemeM :: forall r. (Members '[ExactPrint] r) => Interval -> Sem r () -> Sem r ()
morphemeM loc doc = do
  void (printCommentsUntil loc)
  doc

tuple :: (Members '[ExactPrint] r, Foldable l) => l (Sem r ()) -> Sem r ()
tuple = encloseSep (enqueue C.kwParenL) (noLoc C.kwParenR) (noLoc ", ")
