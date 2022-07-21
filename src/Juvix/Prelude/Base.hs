module Juvix.Prelude.Base
  ( module Juvix.Prelude.Base,
    module Control.Applicative,
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
    module System.Directory,
    module System.Exit,
    module System.FilePath,
    module System.IO,
    module Text.Show,
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
  )
where

import Control.Applicative
import Control.Monad.Extra
import Control.Monad.Fix
import Data.Bifunctor hiding (first, second)
import Data.Bitraversable
import Data.Bool
import Data.ByteString.Lazy (ByteString)
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
import Data.List.Extra hiding (groupSortOn, head, last, mconcatMap)
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
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Semigroup (Semigroup, (<>))
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
import Lens.Micro.Platform hiding (both, _head)
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
import System.Directory
import System.Exit
import System.FilePath
import System.IO hiding
  ( appendFile,
    getContents,
    getLine,
    hGetContents,
    hGetLine,
    hPutStr,
    hPutStrLn,
    interact,
    putStr,
    putStrLn,
    readFile,
    readFile',
    writeFile,
  )
import Text.Show (Show)
import Text.Show qualified as Show

--------------------------------------------------------------------------------
-- High-level syntax sugar.
--------------------------------------------------------------------------------

-- Lift a map.
(|<<) ::
  forall a b f.
  (Functor f) =>
  (a -> b) ->
  -----------
  f a ->
  f b
(|<<) = fmap

infixr 1 |<<

-- Apply a lifted map.
(>>|) :: forall a b f. (Functor f) => f a -> (a -> b) -> f b
(>>|) = flip fmap

infixl 1 >>|

-- Postfix funciton application.
(|>) :: a -> (a -> b) -> b
(|>) = (&)

infixl 1 |>

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

mconcatMap :: (Monoid c, Foldable t) => (a -> c) -> t a -> c
mconcatMap f = List.mconcatMap f . toList

--------------------------------------------------------------------------------
-- HashMap
--------------------------------------------------------------------------------

tableInsert ::
  Hashable k =>
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
-- NonEmpty
--------------------------------------------------------------------------------

nonEmptyUnsnoc :: NonEmpty a -> (Maybe (NonEmpty a), a)
nonEmptyUnsnoc e = (NonEmpty.nonEmpty (NonEmpty.init e), NonEmpty.last e)

_nonEmpty :: Lens' [a] (Maybe (NonEmpty a))
_nonEmpty f x = maybe [] toList <$> f (nonEmpty x)

groupSortOn :: Ord b => (a -> b) -> [a] -> [NonEmpty a]
groupSortOn f = map (fromJust . nonEmpty) . List.groupSortOn f

groupSortOn' :: Ord b => (a -> b) -> [a] -> [[a]]
groupSortOn' = List.groupSortOn

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

error :: HasCallStack => Text -> a
error = Err.error . unpack

{-# DEPRECATED undefined "undefined" #-}
undefined :: HasCallStack => a
undefined = Err.error "undefined"

-- | Used to indicate impossible corner cases.
impossible :: HasCallStack => a
impossible = Err.error "impossible"

--------------------------------------------------------------------------------

infixl 7 <+?>

(<+?>) :: Doc ann -> Maybe (Doc ann) -> Doc ann
(<+?>) a = maybe a (a <+>)

infixl 7 <?+>

(<?+>) :: Maybe (Doc ann) -> Doc ann -> Doc ann
(<?+>) = \case
  Nothing -> id
  Just a -> (a <+>)

infixl 7 <?>

(<?>) :: Semigroup m => m -> Maybe m -> m
(<?>) a = maybe a (a <>)

--------------------------------------------------------------------------------

data Indexed a = Indexed
  { _indexedIx :: Int,
    _indexedThing :: a
  }
  deriving stock (Show, Eq, Ord, Foldable, Traversable)

instance Functor Indexed where
  fmap f (Indexed i a) = Indexed i (f a)

makeLenses ''Indexed

fromText :: IsString a => Text -> a
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

nubHashable :: Hashable a => [a] -> [a]
nubHashable = HashSet.toList . HashSet.fromList

allElements :: (Bounded a, Enum a) => [a]
allElements = [minBound .. maxBound]

readerState :: forall a r x. (Member (State a) r) => Sem (Reader a ': r) x -> Sem r x
readerState m = get >>= (`runReader` m)
