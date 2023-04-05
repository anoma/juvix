module Mari.Library.PrettyPrint
  ( module Text.PrettyPrint.Compact,

    -- * Precedence
    Prec (..),
    PrecReader,

    -- * Classes
    Ann,
    PrettySyntax (..),
    PrettyText (..),
    runPretty,
    runPretty0,
    pretty,
    pretty0,
    withPrec,
    show,
    string,
    text,

    -- * Extensions
    renderIO,
    prettyIO,
    prettyTIO,
    noAnn,
    annotate',
    hangs,
    hangsA,
    hangsWith,
    parens,
    parens',
    parensP,
    parensP',
    app,
    app',
    indentWidth,
    sepIndent,
    sepIndent',
    sepIndentA,
    sepIndentA',
    punctuate,

    -- ** Combinators lifted over an 'Applicative'
    hsepA,
    sepA,
    hcatA,
    vcatA,
    punctuateA,
    hangA,
  )
where

import Data.Text (unpack)
import Mari.Library hiding (show)
import Text.PrettyPrint.Compact hiding
  ( angles,
    backslash,
    braces,
    brackets,
    colon,
    comma,
    dot,
    dquote,
    equals,
    langle,
    lbrace,
    lbracket,
    lparen,
    parens,
    punctuate,
    rangle,
    rbrace,
    rbracket,
    rparen,
    semi,
    space,
    squote,
    string,
    text,
  )
import qualified Text.PrettyPrint.Compact as Base
import Text.PrettyPrint.Compact.Core (groupingBy)
import qualified Text.Show as Show
import Prelude (String)

-- | Annotations, which can be used for e.g. syntax highlighting
type family Ann a :: *

data Prec
  = -- | Outermost expression; no parens needed
    Outer
  | -- | Argument of infix function, with the same meaning as the argument of
    -- 'Text.Show.showsPrec'
    Infix Natural
  | -- | Argument (or head) of nonfix function; all non-atomic expressions need
    -- parens
    FunArg
  deriving (Eq, Ord, Show, Generic)

type PrecReader = HasReader "prec" Prec

-- | Class for pretty-printing syntax-like types, which need to keep track of
-- the surrounding precedence level.
class Monoid (Ann a) => PrettySyntax a where
  -- | Pretty-prints a syntax value, given the current precedence value in
  -- a reader environment.
  pretty' :: PrecReader m => a -> m (Doc (Ann a))
  default pretty' ::
    (PrettyText a, PrecReader m) => a -> m (Doc (Ann a))
  pretty' = pure . prettyT

-- | Class for text-like types (e.g. messages), which don't have a concept of
-- precedence.
class Monoid (Ann a) => PrettyText a where
  -- | Pretty-print a value as human-readable text.
  prettyT :: a -> Doc (Ann a)
  default prettyT :: Show a => a -> Doc (Ann a)
  prettyT = string . Show.show

runPretty :: Prec -> (forall m. PrecReader m => m a) -> a
runPretty prec m = let MonadReader x = m in runReader x prec

runPretty0 :: (forall m. PrecReader m => m a) -> a
runPretty0 = runPretty Outer

-- | Pretty-print at the given precedence level.
pretty :: PrettySyntax a => Prec -> a -> Doc (Ann a)
pretty prec x = runPretty prec (pretty' x)

-- | Pretty-print at the initial precedence level.
pretty0 :: PrettySyntax a => a -> Doc (Ann a)
pretty0 = pretty Outer

withPrec :: PrecReader m => Prec -> m a -> m a
withPrec p = local @"prec" \_ -> p

show :: (Monoid ann, Show a) => a -> Doc ann
show = string . Show.show

string :: Monoid ann => String -> Doc ann
string = Base.text

text :: Monoid ann => Text -> Doc ann
text = string . unpack

noAnn :: Monoid ann2 => Doc ann1 -> Doc ann2
noAnn d = mempty <$ d

hsepA ::
  (Applicative f, Foldable t, Monoid ann) =>
  t (f (Doc ann)) ->
  f (Doc ann)
hsepA = fmap hsep . sequenceA . toList

sepA ::
  (Applicative f, Foldable t, Monoid ann) =>
  t (f (Doc ann)) ->
  f (Doc ann)
sepA = fmap sep . sequenceA . toList

hcatA ::
  (Applicative f, Foldable t, Monoid ann) =>
  t (f (Doc ann)) ->
  f (Doc ann)
hcatA = fmap hcat . sequenceA . toList

vcatA ::
  (Applicative f, Foldable t, Monoid ann) =>
  t (f (Doc ann)) ->
  f (Doc ann)
vcatA = fmap vcat . sequenceA . toList

-- Same as 'Base.punctuate' but accepts any 'Foldable' type.
punctuate :: (Foldable t, Monoid ann) => Doc ann -> t (Doc ann) -> [Doc ann]
punctuate s = Base.punctuate s . toList

punctuateA ::
  (Applicative f, Foldable t, Monoid ann) =>
  f (Doc ann) ->
  t (f (Doc ann)) ->
  [f (Doc ann)]
punctuateA s = go . toList
  where
    go [] = []
    go [d] = [d]
    go (d : ds) = hcatA [d, s] : go ds

hangA ::
  (Applicative f, Monoid ann) =>
  Int ->
  f (Doc ann) ->
  f (Doc ann) ->
  f (Doc ann)
hangA i = liftA2 $ hang i

-- | Same as 'hangWith', but with multiple hanging elements.
--
-- @
-- >>> hangsWith "*" 2 "hello" ["cool", "world"]
-- hello*cool*world
-- -- or --
-- hello
--   cool
--   world
-- @
hangsWith ::
  (Foldable t, Monoid ann) =>
  String ->
  Int ->
  Doc ann ->
  t (Doc ann) ->
  Doc ann
hangsWith sep n a bs =
  groupingBy sep $ (0, a) : map (n,) (toList bs)

hangs :: (Foldable t, Monoid ann) => Int -> Doc ann -> t (Doc ann) -> Doc ann
hangs = hangsWith " "

hangsA ::
  (Applicative f, Foldable t, Monoid ann) =>
  Int ->
  f (Doc ann) ->
  t (f (Doc ann)) ->
  f (Doc ann)
hangsA i a bs = hangs i <$> a <*> sequenceA (toList bs)

-- | Surrounds a doc with parens with the given annotation
parens :: Monoid ann => ann -> Doc ann -> Doc ann
parens ann = annotate ann "(" `enclose` annotate ann ")"

parens' :: ann -> Doc (Last ann) -> Doc (Last ann)
parens' = parens . Last . Just

-- | Surrounds with parens if the current precedence level is less than the
-- given one (the same behaviour as 'Text.Show.showsPrec').
parensP ::
  (Monoid ann, PrecReader m) => ann -> Prec -> m (Doc ann) -> m (Doc ann)
parensP ann p d = do
  p' <- ask @"prec"
  if p > p' then d else parens ann <$> d

parensP' ::
  PrecReader m => ann -> Prec -> m (Doc (Last ann)) -> m (Doc (Last ann))
parensP' = parensP . Last . Just

-- | Usually our annotations are 'Last' of some syntax highlighting
-- representation.
annotate' :: ann -> Doc (Last ann) -> Doc (Last ann)
annotate' = annotate . Last . Just

-- | Formats an application, by aligning the arguments and adjusting the
-- precedence level of the subterms.
--
-- @
-- fun arg1 arg2 arg3
-- -- or --
-- fun
--   arg1
--   arg2
--   arg3
-- @
app ::
  ( PrecReader m,
    Foldable t,
    Monoid ann
  ) =>
  ann ->
  m (Doc ann) ->
  t (m (Doc ann)) ->
  m (Doc ann)
app ann f xs = parensP ann FunArg $ withPrec FunArg $ hangsA indentWidth f xs

-- | Same as 'app' but with a 'Last' for an annotation.
app' ::
  ( PrecReader m,
    Foldable t
  ) =>
  ann ->
  m (Doc (Last ann)) ->
  t (m (Doc (Last ann))) ->
  m (Doc (Last ann))
app' = app . Last . Just

-- | Default indent width.
indentWidth :: Int
indentWidth = 2

-- | Like 'sep', but indenting each item the given amount if they are laid out
-- vertically.
sepIndent :: (Monoid ann, Foldable t) => t (Int, Doc ann) -> Doc ann
sepIndent = groupingBy " " . toList

-- | Like 'sepIndent', indenting by 'indentWidth' if the first part is
-- 'True'
sepIndent' :: (Monoid ann, Foldable t) => t (Bool, Doc ann) -> Doc ann
sepIndent' = sepIndent . map (first toIndent) . toList

toIndent :: Bool -> Int
toIndent b = if b then indentWidth else 0

sepIndentA ::
  (Monoid ann, Foldable t, Applicative f) =>
  t (Int, f (Doc ann)) ->
  f (Doc ann)
sepIndentA = fmap (groupingBy " ") . sequenceA . map sequenceA . toList

sepIndentA' ::
  (Monoid ann, Foldable t, Functor t, Applicative f) =>
  t (Bool, f (Doc ann)) ->
  f (Doc ann)
sepIndentA' = sepIndentA . fmap (first toIndent)

renderIO :: (MonadIO m, Monoid ann) => Doc ann -> m ()
renderIO = putStrLn . render

prettyIO :: (MonadIO m, PrettySyntax a) => a -> m ()
prettyIO = renderIO . pretty0

prettyTIO :: (MonadIO m, PrettyText a) => a -> m ()
prettyTIO = renderIO . prettyT

-- TODO: use syntax highlighting

type instance Ann Void = ()

instance PrettySyntax Void

instance PrettyText Void where prettyT = absurd
