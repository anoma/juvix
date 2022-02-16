module MiniJuvix.Prelude.Base 
  ( module MiniJuvix.Prelude.Base,
    module Control.Monad.Extra,
    module Data.Char,
    module Data.Typeable,
    module Data.Either.Extra,
    module Data.Function,
    module Data.List.Extra,
    module Data.Maybe,
    module Data.String,
    module Data.Text.Encoding,
    module GHC.Real,
    module Data.Tuple.Extra,
    module Data.Void,
    module GHC.Enum,
    module System.Directory,
    module System.FilePath,
    module Data.Singletons,
    module Data.Singletons.TH,
    module Data.Singletons.Sigma,
    module Data.Hashable,
    module Lens.Micro.Platform,
    module GHC.Generics,
    module Data.Bool,
    module Data.List.NonEmpty,
    module Data.Traversable,
    module Data.Monoid,
    module Polysemy,
    module Polysemy.Reader,
    module Data.Text.IO,
    module Polysemy.State,
    module Polysemy.Error,
    module Polysemy.Embed,
    module Text.Show,
    module Data.Eq,
    module Data.Ord,
    module Data.Semigroup,
    module Data.Stream,
    module GHC.Num,
    module Data.Word,
    module Data.Functor,
    module Data.Int,
    module Polysemy.View,
    module System.Exit,
    module System.IO,
    module Control.Applicative,
    module Data.Foldable,
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
import Data.Typeable hiding (TyCon)
import Control.Monad.Extra
import Data.Bool
import Data.ByteString.Lazy (ByteString)
import Data.Char
import qualified Data.Char as Char
import Data.Data
import Data.Either.Extra
import Data.Eq
import Data.Foldable
import Data.Function
import Data.Functor
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Hashable
import Data.Int
import Data.List.Extra hiding (head, last)
import Data.List.NonEmpty (NonEmpty (..), head, last, nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe
import Data.Singletons.Sigma
import Data.Monoid
import Data.Ord
import Data.Semigroup (Semigroup, (<>))
import Data.Singletons
import Data.Singletons.TH (genSingletons)
import Data.Stream (Stream)
import Data.String
import Data.Text (Text, pack, strip, unpack)
import Data.Text.Encoding
import Data.Traversable
import Data.Tuple.Extra
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
import Polysemy.Reader
import Polysemy.State
import Polysemy.View
import System.Directory
import System.Exit
import System.FilePath
import System.IO hiding (putStr, putStrLn, hPutStr, hPutStrLn, writeFile, hGetContents, interact, readFile, getContents, getLine, appendFile, hGetLine)
import Text.Show (Show)
import Data.Text.IO
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
-- Semiring
--------------------------------------------------------------------------------

class Monoid m => Semiring m where
  {-# MINIMAL one, times #-}

  one :: m
  times :: m -> m -> m

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

