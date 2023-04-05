{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- - The standard Library for the project
--   + Thus all code will depend on this module without stating otherwise
-- - Is mostly =Protolude= except with a few changes
--   + _Additions_
--     * ∨   :: Serves as an or function
--     * ∧   :: Serves as an and function
--     * |<< :: Serves as a map function
--     * >>| :: Serves as the flip map function
--   + _Changes_
--     * The Capability library is imported and replaces the standard =MTL=
--       constructs in =Protolude=
module Mari.Library
  ( module Protolude,
    module Capability.State,
    module Capability.Reader,
    module Capability.Error,
    module Capability.Writer,
    module Capability.Sink,
    module Capability.Source,
    module Numeric.Natural,
    undefined,
    Data,
    (∨),
    (∧),
    (|<<),
    (>>|),
    (|>),
    (...),
    traverseAccumM,
    foldMapA,
    traverseM,
    Symbol (..),
    toUpperFirst,
    internText,
    intern,
    unintern,
    textify,
    uninternText,
    unixTime,
    Flip (..),
    untilNothingNTimesM,
    untilNothing,
    sortOnFlip,
    uncurry3,
    curry3,
    dup,
    lengthN,
    StateField,
    ReaderField,
    WriterField,
    init,
  )
where

import Capability.Error
import Capability.Reader
import Capability.Sink
import Capability.Source
import Capability.State
import Capability.Writer
import qualified Data.Aeson as A
import Data.Data (Data)
import qualified Data.List.NonEmpty as NonEmpty
import Data.String (fromString)
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import GHC.Stack
import Numeric.Natural
import Protolude hiding
  ( Constraint,
    Fixity (..),
    MonadError (..),
    MonadReader (..),
    MonadState (..),
    SomeSymbol,
    Symbol,
    ask,
    asks,
    catch,
    catchJust,
    get,
    gets,
    isDigit,
    isSpace,
    local,
    modify,
    moduleName,
    pass,
    put,
    reader,
    state,
    typeOf,
    undefined,
    yield,
    (:.:),
  )
import Prelude (Show (..), String, error, init)

(∨) :: Bool -> Bool -> Bool
(∨) = (||)

infixr 2 ∨

(∧) :: Bool -> Bool -> Bool
(∧) = (&&)

infixr 3 ∧

(|<<) :: forall a b f. (Functor f) => (a -> b) -> f a -> f b
(|<<) = fmap

infixr 1 |<<

(>>|) :: forall a b f. (Functor f) => f a -> (a -> b) -> f b
(>>|) = flip fmap

infixl 1 >>|

(|>) :: a -> (a -> b) -> b
(|>) = (&)

infixl 1 |>

undefined :: HasCallStack => a
undefined =
  Prelude.error $ "undefined\n" ++ prettyCallStack callStack

--------------------------------------------------------------------------------
-- Generic Traversal Function
--------------------------------------------------------------------------------

traverseAccumM ::
  (Monad m, Traversable t) => (a -> b -> m (a, c)) -> a -> t b -> m (a, t c)
traverseAccumM f init trav =
  runStateT (traverse (StateT . newF) trav) init
    >>| swap
  where
    newF b a = f a b >>| swap

traverseM ::
  (Monad m, Traversable m, Applicative f) =>
  (a1 -> f (m a2)) ->
  m a1 ->
  f (m a2)
traverseM f = fmap join . traverse f

foldMapA ::
  (Applicative f, Foldable t, Monoid a) =>
  (b -> f a) ->
  t b ->
  f a
foldMapA f = foldl' (\acc x -> liftA2 (<>) acc (f x)) (pure mempty)

instance Show (a -> b) where
  show _ = "fun"

newtype Symbol = Sym Text
  deriving newtype (Eq, Show, Read, Hashable, Semigroup, Ord, NFData, A.ToJSON, A.FromJSON, A.ToJSONKey, A.FromJSONKey)
  deriving stock (Data, Generic)

instance IsString Symbol where
  fromString = intern

toUpperFirst :: String -> String
toUpperFirst [] = []
toUpperFirst (x : xs) = toUpper x : xs

internText :: Text -> Symbol
internText = Sym

intern :: String -> Symbol
intern = Sym . T.pack

unintern :: Symbol -> String
unintern (Sym s) = T.unpack s

uninternText :: Symbol -> Text
uninternText (Sym s) = s

textify :: Symbol -> Text
textify (Sym s) = s

unixTime :: IO Double
unixTime = fromRational . realToFrac |<< getPOSIXTime

newtype Flip p a b = Flip {runFlip :: p b a}
  deriving (Show, Generic, Eq, Ord, Typeable)

untilNothingNTimesM :: (Num t, Ord t, Enum t, Monad f) => f Bool -> t -> f ()
untilNothingNTimesM f n
  | n <= 0 = pure ()
  | otherwise =
    f >>= \case
      True -> untilNothingNTimesM f (pred n)
      False -> pure ()

untilNothing :: (t -> Maybe t) -> t -> t
untilNothing f a = case f a of
  Nothing -> a
  Just a -> untilNothing f a

-- | like sortOn from the stdlib, is an optimized version of `sortBy (comparing f)`
-- However instead of sorting from lowest to highest, this sorts from higher to lowest
sortOnFlip :: Ord b => (a -> b) -> [a] -> [a]
sortOnFlip f =
  fmap snd . sortBy (flip (comparing fst))
    . fmap
      ( \x ->
          let y = f x
           in y `seq` (y, x)
      )

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 fn (a, b, c) = fn a b c

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 fn a b c = fn (a, b, c)

(...) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(...) = (.) . (.)

dup :: a -> (a, a)
dup x = (x, x)

-- | Same as 'length' but returning a 'Natural'.
lengthN :: Foldable f => f a -> Natural
lengthN = foldl' (\n _ -> n + 1) 0

-- | Select a field in a state monad, for example:
--
-- @
-- data Foo = Foo {x, y :: 'Int'}
-- newtype M a = M ('State' 'Foo' a)
--   deriving ('HasState' \"x\" 'Int') via StateField "x" ('State' 'Foo')
-- @
type StateField fld m = Field fld () (MonadState m)

-- | Reader version of 'StateField'.
type ReaderField fld m = ReadStatePure (StateField fld m)

-- | Writer version of 'StateField'.
type WriterField fld m = WriterLog (StateField fld m)

instance (A.ToJSON a) => A.ToJSONKey (NonEmpty.NonEmpty a)

instance (A.FromJSON a) => A.FromJSONKey (NonEmpty.NonEmpty a)
