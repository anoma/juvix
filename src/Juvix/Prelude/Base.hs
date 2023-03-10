{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid restricted extensions" #-}
{-# HLINT ignore "Avoid restricted flags" #-}

module Juvix.Prelude.Base
  ( module Juvix.Prelude.Base,
    module Control.Applicative,
    module Data.Map.Strict,
    module Data.Set,
    module Data.IntMap.Strict,
    module Data.IntSet,
    module Control.Monad.Extra,
    module Control.Monad.Fix,
    module Data.Bitraversable,
    module Data.Bool,
    module Data.Char,
    module Data.Either.Extra,
    module Data.Bifunctor,
    module Data.Eq,
    module Data.Foldable,
    module Data.Function,
    module Data.Functor,
    module Safe.Exact,
    module Safe.Foldable,
    module Data.Hashable,
    module Data.Int,
    module Data.List.Extra,
    module Data.List.NonEmpty.Extra,
    module Data.Maybe,
    module Data.Monoid,
    module Data.Ord,
    module Data.Semigroup,
    module Prelude,
    module Data.Singletons,
    module Data.Singletons.Sigma,
    module Data.Singletons.TH,
    module Data.Stream,
    module Data.String,
    module Data.Text.Encoding,
    module Data.Text.IO,
    module Data.Traversable,
    module Data.Tuple.Extra,
    module Data.Typeable,
    module Data.Void,
    module Data.Word,
    module GHC.Enum,
    module GHC.Generics,
    module GHC.Num,
    module GHC.Real,
    module Lens.Micro.Platform,
    module Polysemy,
    module Polysemy.Embed,
    module Polysemy.Error,
    module Polysemy.Fixpoint,
    module Polysemy.Output,
    module Polysemy.Reader,
    module Polysemy.State,
    module Language.Haskell.TH.Syntax,
    module Prettyprinter,
    module System.Exit,
    module System.FilePath,
    module System.IO,
    module Text.Show,
    module Control.Monad.Catch,
    Data,
    Text,
    pack,
    unpack,
    strip,
    HashMap,
    ByteString,
    HashSet,
    IsString (..),
    Alternative (..),
    MonadIO (..),
  )
where

import Control.Applicative
import Control.Monad.Catch (MonadMask, MonadThrow, throwM)
import Control.Monad.Extra hiding (fail)
import Control.Monad.Fix
import Control.Monad.IO.Class (MonadIO (..))
import Data.Bifunctor hiding (first, second)
import Data.Bitraversable
import Data.Bool
import Data.ByteString (ByteString)
import Data.Char
import Data.Char qualified as Char
import Data.Data
import Data.Either.Extra
import Data.Eq
import Data.Foldable hiding (minimum, minimumBy)
import Data.Function
import Data.Functor
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Hashable
import Data.Int
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Data.List.Extra hiding (allSame, groupSortOn, head, last, mconcatMap)
import Data.List.Extra qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.NonEmpty.Extra
  ( NonEmpty (..),
    head,
    last,
    maximum1,
    maximumOn1,
    minimum1,
    minimumOn1,
    nonEmpty,
    some1,
    (|:),
  )
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Semigroup (Semigroup, (<>))
import Data.Set (Set)
import Data.Singletons hiding ((@@))
import Data.Singletons.Sigma
import Data.Singletons.TH (genSingletons, promoteOrdInstances, singOrdInstances)
import Data.Stream (Stream)
import Data.String
import Data.Text (Text, pack, strip, unpack)
import Data.Text.Encoding
import Data.Text.IO
import Data.Traversable
import Data.Tuple.Extra
import Data.Typeable hiding (TyCon)
import Data.Void
import Data.Word
import GHC.Enum
import GHC.Err qualified as Err
import GHC.Generics (Generic)
import GHC.Num
import GHC.Real
import GHC.Stack.Types
import Language.Haskell.TH.Syntax (Lift)
import Lens.Micro.Platform hiding (both)
import Path
import Path.IO qualified as Path
import Polysemy
import Polysemy.Embed
import Polysemy.Error hiding (fromEither)
import Polysemy.Fixpoint
import Polysemy.Output
import Polysemy.Reader
import Polysemy.State
import Prettyprinter (Doc, (<+>))
import Safe.Exact
import Safe.Foldable
import System.Exit
import System.FilePath (FilePath, dropTrailingPathSeparator, normalise, (<.>), (</>))
import System.IO hiding
  ( appendFile,
    getContents,
    getLine,
    hGetContents,
    hGetLine,
    hPutStr,
    hPutStrLn,
    interact,
    openBinaryTempFile,
    openTempFile,
    putStr,
    putStrLn,
    readFile,
    readFile',
    writeFile,
  )
import System.IO.Error
import Text.Show (Show)
import Text.Show qualified as Show
import Prelude (Double)

--------------------------------------------------------------------------------

traverseM ::
  (Monad m, Traversable m, Applicative f) =>
  (a1 -> f (m a2)) ->
  m a1 ->
  f (m a2)
traverseM f = fmap join . traverse f

--------------------------------------------------------------------------------
-- String related util functions.
--------------------------------------------------------------------------------

show :: (Show a, IsString str) => a -> str
show = fromString . Show.show

toUpperFirst :: String -> String
toUpperFirst [] = []
toUpperFirst (x : xs) = Char.toUpper x : xs

--------------------------------------------------------------------------------
-- Foldable
--------------------------------------------------------------------------------

allSame :: forall t a. (Eq a, Foldable t) => t a -> Bool
allSame t
  | null t = True
  | otherwise = all (== h) t
  where
    h :: a
    h = foldr1 const t

mconcatMap :: (Monoid c, Foldable t) => (a -> c) -> t a -> c
mconcatMap f = List.mconcatMap f . toList

concatWith :: (Foldable t, Monoid a) => (a -> a -> a) -> t a -> a
concatWith f ds
  | null ds = mempty
  | otherwise = foldr1 f ds
{-# INLINE concatWith #-}

--------------------------------------------------------------------------------
-- HashMap
--------------------------------------------------------------------------------

tableInsert ::
  (Hashable k) =>
  (a -> v) ->
  (a -> v -> v) ->
  k ->
  a ->
  HashMap k v ->
  HashMap k v
tableInsert s f k a = over (at k) (Just . aux)
  where
    aux = \case
      Just v -> f a v
      Nothing -> s a

tableNestedInsert ::
  (Hashable k1, Hashable k2) =>
  k1 ->
  k2 ->
  a ->
  HashMap k1 (HashMap k2 a) ->
  HashMap k1 (HashMap k2 a)
tableNestedInsert k1 k2 = tableInsert (HashMap.singleton k2) (HashMap.insert k2) k1

--------------------------------------------------------------------------------
-- List
--------------------------------------------------------------------------------

revAppend :: [a] -> [a] -> [a]
revAppend [] !ys = ys
revAppend (x : xs) !ys = revAppend xs (x : ys)

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (h : t) =
  -- keeping the lets separate ensures that `v` is evaluated before `vs`
  let !v = f h
   in let !vs = map' f t
       in v : vs

-- | longest common prefix
commonPrefix :: forall a. (Eq a) => [a] -> [a] -> [a]
commonPrefix a b = reverse (go [] a b)
  where
    go :: [a] -> [a] -> [a] -> [a]
    go ac x y = case (x, y) of
      (x' : xs, y' : ys)
        | x' == y' -> go (x' : ac) xs ys
      _ -> ac

zip4Exact :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4Exact [] [] [] [] = []
zip4Exact (x1 : t1) (x2 : t2) (x3 : t3) (x4 : t4) = (x1, x2, x3, x4) : zip4Exact t1 t2 t3 t4
zip4Exact _ _ _ _ = error "zip4Exact"

--------------------------------------------------------------------------------
-- NonEmpty
--------------------------------------------------------------------------------

nonEmptyUnsnoc :: NonEmpty a -> (Maybe (NonEmpty a), a)
nonEmptyUnsnoc e = (NonEmpty.nonEmpty (NonEmpty.init e), NonEmpty.last e)

nonEmpty' :: HasCallStack => [a] -> NonEmpty a
nonEmpty' = fromJust . nonEmpty

_nonEmpty :: Lens' [a] (Maybe (NonEmpty a))
_nonEmpty f x = maybe [] toList <$> f (nonEmpty x)

groupSortOn :: (Ord b) => (a -> b) -> [a] -> [NonEmpty a]
groupSortOn f = map (fromJust . nonEmpty) . List.groupSortOn f

groupSortOn' :: (Ord b) => (a -> b) -> [a] -> [[a]]
groupSortOn' = List.groupSortOn

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

error :: (HasCallStack) => Text -> a
error = Err.error . unpack

{-# DEPRECATED undefined "undefined" #-}
undefined :: (HasCallStack) => a
undefined = Err.error "undefined"

-- | Used to indicate impossible corner cases.
impossible :: (HasCallStack) => a
impossible = Err.error "impossible"

--------------------------------------------------------------------------------

infixl 7 <+?>

(<+?>) :: Doc ann -> Maybe (Doc ann) -> Doc ann
(<+?>) a = maybe a (a <+>)

infixr 7 <?+>

(<?+>) :: Maybe (Doc ann) -> Doc ann -> Doc ann
(<?+>) = \case
  Nothing -> id
  Just a -> (a <+>)

infixr 7 ?<>

(?<>) :: (Semigroup m) => Maybe m -> m -> m
(?<>) = maybe id (<>)

infixl 7 <>?

(<>?) :: (Semigroup m) => m -> Maybe m -> m
(<>?) a = maybe a (a <>)

data Indexed a = Indexed
  { _indexedIx :: Int,
    _indexedThing :: a
  }
  deriving stock (Show, Eq, Ord, Foldable, Traversable)

instance Functor Indexed where
  fmap f (Indexed i a) = Indexed i (f a)

indexFrom :: Int -> [a] -> [Indexed a]
indexFrom i = zipWith Indexed [i ..]

makeLenses ''Indexed

toTuple :: Indexed a -> (Int, a)
toTuple i = (i ^. indexedIx, i ^. indexedThing)

filterIndexed :: (a -> Bool) -> [Indexed a] -> [Indexed a]
filterIndexed f = filter (f . (^. indexedThing))

fromText :: (IsString a) => Text -> a
fromText = fromString . unpack

fromRightIO' :: (e -> IO ()) -> IO (Either e r) -> IO r
fromRightIO' pp = do
  eitherM ifLeft return
  where
    ifLeft e = pp e >> exitFailure

fromRightIO :: (e -> Text) -> IO (Either e r) -> IO r
fromRightIO pp = fromRightIO' (putStrLn . pp)

--------------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------------

-- | applies a function n times
iterateN :: Int -> (a -> a) -> a -> a
iterateN n f = (!! n) . iterate f

nubHashable :: (Hashable a) => [a] -> [a]
nubHashable = HashSet.toList . HashSet.fromList

allElements :: (Bounded a, Enum a) => [a]
allElements = [minBound .. maxBound]

infixr 3 .&&.

(.&&.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(f .&&. g) a = f a && g a

infixr 3 ..&&..

(..&&..) :: (a -> b -> Bool) -> (a -> b -> Bool) -> (a -> b -> Bool)
(f ..&&.. g) a = f a .&&. g a

infixr 2 .||.

(.||.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(a .||. b) c = a c || b c

eqOn :: (Eq b) => (a -> b) -> a -> a -> Bool
eqOn = ((==) `on`)

class CanonicalProjection a b where
  project :: a -> b

instance CanonicalProjection a a where
  project = id

instance CanonicalProjection Void a where
  project = absurd

instance CanonicalProjection a () where
  project = const ()

-- | 'project' with type arguments swapped. Useful for type application
project' :: forall b a. (CanonicalProjection a b) => a -> b
project' = project

ensureFile :: (MonadIO m, MonadThrow m) => Path Abs File -> m ()
ensureFile f =
  unlessM
    (Path.doesFileExist f)
    (throwM (mkIOError doesNotExistErrorType "" Nothing (Just (toFilePath f))))
