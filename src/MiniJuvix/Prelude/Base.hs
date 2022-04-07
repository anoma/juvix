module MiniJuvix.Prelude.Base
  ( module MiniJuvix.Prelude.Base,
    module Control.Applicative,
    module Control.Monad.Extra,
    module Control.Monad.Fix,
    module Data.Bool,
    module Data.Char,
    module Data.Either.Extra,
    module Data.Eq,
    module Data.Foldable,
    module Data.Function,
    module Data.Functor,
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

--------------------------------------------------------------------------------

import Control.Applicative
import Control.Monad.Extra
import Control.Monad.Fix
import Data.Bool
import Data.ByteString.Lazy (ByteString)
import Data.Char
import qualified Data.Char as Char
import Data.Data
import Data.Either.Extra
import Data.Eq
import Data.Foldable hiding (minimum, minimumBy)
import Data.Function
import Data.Functor
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable
import Data.Int
import Data.List.Extra hiding (head, last)
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty.Extra (NonEmpty (..), head, last, maximum1, maximumOn1, minimum1, minimumOn1, nonEmpty, some1)
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Semigroup (Semigroup, (<>))
import Data.Singletons
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
import qualified GHC.Err as Err
import GHC.Generics (Generic)
import GHC.Num
import GHC.Real
import GHC.Stack.Types
import Lens.Micro.Platform hiding (both)
import Polysemy
import Polysemy.Embed
import Polysemy.Error hiding (fromEither)
import Polysemy.Fixpoint
import Polysemy.Output
import Polysemy.Reader
import Polysemy.State
import Prettyprinter (Doc, (<+>))
import System.Directory
import System.Exit
import System.FilePath
import System.IO hiding (appendFile, getContents, getLine, hGetContents, hGetLine, hPutStr, hPutStrLn, interact, putStr, putStrLn, readFile, readFile', writeFile)
import Text.Show (Show)
import qualified Text.Show as Show

--------------------------------------------------------------------------------
-- Logical connectives
--------------------------------------------------------------------------------

(∨) :: Bool -> Bool -> Bool
(∨) = (||)

infixr 2 ∨

(∧) :: Bool -> Bool -> Bool
(∧) = (&&)

infixr 3 ∧

(.||.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(f .||. g) x = f x || g x

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
-- NonEmpty
--------------------------------------------------------------------------------

nonEmptyUnsnoc :: NonEmpty a -> (Maybe (NonEmpty a), a)
nonEmptyUnsnoc e = (NonEmpty.nonEmpty (NonEmpty.init e), NonEmpty.last e)

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

error :: HasCallStack => Text -> a
error = Err.error . unpack

undefined :: HasCallStack => a
undefined = Err.error "undefined"

-- | Used to indicate impossible corner cases.
impossible :: HasCallStack => a
impossible = Err.error "impossible"

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

infixl 7 <+?>

(<+?>) :: Doc ann -> Maybe (Doc ann) -> Doc ann
(<+?>) a = maybe a (a <+>)

infixl 7 <?>

(<?>) :: Semigroup m => m -> Maybe m -> m
(<?>) a = maybe a (a <>)

data Indexed a = Indexed
  { _indexedIx :: Int,
    _indexedThing :: a
  }
  deriving stock (Show, Eq, Ord, Foldable, Traversable)

instance Functor Indexed where
  fmap f (Indexed i a) = Indexed i (f a)

makeLenses ''Indexed

minimumMaybe :: (Foldable t, Ord a) => t a -> Maybe a
minimumMaybe l = if null l then Nothing else Just (minimum l)

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
