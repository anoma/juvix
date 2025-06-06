{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Juvix.Prelude.Base.Foundation
  ( module Juvix.Prelude.Base.Foundation,
    module Control.Applicative,
    module Control.DeepSeq,
    module System.IO.Error,
    module Control.Lens,
    module Control.Monad.Catch,
    module Control.Monad.Extra,
    module Control.Monad.Fix,
    module Control.Monad.Zip,
    module Data.Bifunctor,
    module Data.Bitraversable,
    module Data.Bool,
    module Data.Char,
    module Data.Either.Extra,
    module Data.Eq,
    module Data.Foldable,
    module Data.Function,
    module Data.Functor,
    module Data.Functor.Identity,
    module Data.Graph,
    module Data.Hashable,
    module Data.Int,
    module Data.IntMap.Strict,
    module Data.IntSet,
    module Data.List.Extra,
    module Data.List.NonEmpty.Extra,
    module Data.Map.Strict,
    module Data.Maybe,
    module Data.Monoid,
    module Data.Ord,
    module Data.Semigroup,
    module Data.Serialize,
    module Data.Set,
    module Data.Singletons,
    module Data.Singletons.Sigma,
    module Data.Singletons.TH,
    module Data.Stream,
    module Data.String,
    module Data.String.Interpolate,
    module Data.Text.Encoding,
    module Data.Text.IO,
    module Data.Text.IO.Utf8,
    module Data.Traversable,
    module Data.Tree,
    module Data.Tuple.Extra,
    module Data.Typeable,
    module Data.Versions,
    module Data.Void,
    module Data.Word,
    module GHC.Conc,
    module GHC.Enum,
    module GHC.Generics,
    module GHC.Num,
    module GHC.Real,
    module GHC.Utils.Misc,
    module Language.Haskell.TH.Syntax,
    module Numeric,
    module Path,
    module Prelude,
    module Prettyprinter,
    module Safe,
    module Safe.Exact,
    module Safe.Foldable,
    module System.Exit,
    module System.FilePath,
    module System.IO,
    module Text.Read,
    module Text.Show,
    module Text.Show.Unicode,
    module Data.Bits,
    Data,
    Text,
    pack,
    unpack,
    strip,
    assert,
    HashMap,
    ByteString,
    HashSet,
    IsString (..),
    Alternative (..),
    MonadIO (..),
    type (~),
  )
where

import Control.Applicative
import Control.DeepSeq
import Control.Lens hiding
  ( Context,
    Index,
    Indexed,
    Level,
    List,
    argument,
    au,
    below,
    children,
    cons,
    cosmos,
    enum,
    from,
    holes,
    ignored,
    imapM,
    indexed,
    indices,
    inside,
    op,
    para,
    parts,
    pre,
    re,
    repeated,
    rmap,
    snoc,
    uncons,
    universe,
    unsnoc,
    (#),
    (<.>),
  )
import Control.Monad.Catch (ExitCase (..), MonadMask, MonadThrow, SomeException, generalBracket, throwM)
import Control.Monad.Extra hiding (fail, forM, mconcatMapM, whileJustM)
import Control.Monad.Extra qualified as Monad
import Control.Monad.Fix
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Zip
import Data.Array qualified as Array
import Data.Bifunctor hiding (first, second)
import Data.Bitraversable
import Data.Bits hiding (And, shift)
import Data.Bool
import Data.ByteString (ByteString)
import Data.Char
import Data.Char qualified as Char
import Data.Data
import Data.Either.Extra
import Data.Eq
import Data.Foldable hiding (foldr1, minimum, minimumBy)
import Data.Function
import Data.Functor
import Data.Functor.Identity
import Data.Graph (Graph, SCC (..), Vertex, scc, stronglyConnComp)
import Data.Graph qualified as Graph
import Data.HashMap.Lazy qualified as LazyHashMap
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Hashable
import Data.Int
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind qualified as GHC
import Data.List.Extra hiding (allSame, foldr1, groupSortOn, head, last, mconcatMap, replicate, unzip)
import Data.List.Extra qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.NonEmpty.Extra
  ( NonEmpty (..),
    appendList,
    head,
    last,
    maximum1,
    maximumOn1,
    minimum1,
    minimumOn1,
    prependList,
    some1,
    (|:),
  )
import Data.Map qualified as Map
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Primitive.ByteArray qualified as GHCByteArray
import Data.Semigroup (Semigroup, sconcat, (<>))
import Data.Serialize (Serialize)
import Data.Serialize as Serial
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Singletons hiding ((@@))
import Data.Singletons.Sigma
import Data.Singletons.TH (genDefunSymbols, genSingletons, promoteOrdInstances, singOrdInstances)
import Data.Stream (Stream)
import Data.Stream qualified as Stream
import Data.String
import Data.String.Interpolate (__i)
import Data.Text (Text, pack, strip, unpack)
import Data.Text qualified as Text
import Data.Text.Encoding
import Data.Text.IO hiding (appendFile, getContents, getLine, hGetContents, hGetLine, hPutStr, hPutStrLn, interact, putStr, putStrLn, readFile, writeFile)
import Data.Text.IO qualified as Text
import Data.Text.IO.Utf8 hiding (getContents, getLine, hGetLine, hPutStr, hPutStrLn, putStr, putStrLn, readFile, writeFile)
import Data.Text.IO.Utf8 qualified as Utf8
import Data.Text.Lazy.Builder qualified as LazyText
import Data.Traversable
import Data.Tree hiding (levels)
import Data.Tuple.Extra hiding (both)
import Data.Type.Equality (type (~))
import Data.Typeable hiding (TyCon)
import Data.Versions (SemVer (..), Versioning (..))
import Data.Versions qualified as Versions
import Data.Void
import Data.Word
import GHC.Base (assert)
import GHC.Conc (ThreadId)
import GHC.Enum
import GHC.Err qualified as Err
import GHC.Generics (Generic)
import GHC.Num
import GHC.Real
import GHC.Stack qualified as GHC
import GHC.Stack.Types
import GHC.Utils.Misc (isSingleton)
import Language.Haskell.TH.Syntax (Exp, Lift, Q)
import Numeric hiding (exp, log, pi)
import Path (Abs, Dir, File, Path, Rel, SomeBase (..))
import Path qualified as PPath
import Path.IO qualified as Path hiding (getCurrentDir, setCurrentDir, withCurrentDir)
import Prettyprinter (Doc, (<+>))
import Safe (headMay)
import Safe.Exact
import Safe.Foldable
import System.Exit hiding (exitFailure, exitSuccess)
import System.Exit qualified as IO
import System.FilePath (FilePath, dropTrailingPathSeparator, normalise, splitDirectories, (<.>), (</>))
import System.FilePath qualified as FilePath
import System.IO hiding
  ( appendFile,
    getContents,
    getLine,
    hClose,
    hFlush,
    hGetContents,
    hGetLine,
    hIsEOF,
    hPutStr,
    hPutStrLn,
    interact,
    openBinaryTempFile,
    openTempFile,
    print,
    putStr,
    putStrLn,
    readFile,
    readFile',
    writeFile,
  )
import System.IO qualified as IO
import System.IO.Error
import Text.Read (Read, readEither)
import Text.Read qualified as Text
import Text.Show (Show)
import Text.Show qualified as Show
import Text.Show.Unicode (urecover, ushow)
import Prelude (Double)

type TextBuilder = LazyText.Builder

type GHCType = GHC.Type

type GHCByteArray = GHCByteArray.ByteArray

type GHCConstraint = GHC.Constraint

type LazyHashMap = LazyHashMap.HashMap

type SimpleFold s a = forall r. (Monoid r) => Getting r s a

type SimpleGetter s a = forall r. Getting r s a

ghcCallStack :: (HasCallStack) => Text
ghcCallStack = pack (GHC.prettyCallStack GHC.callStack)

readJust :: (Read a) => String -> a
readJust = Text.read

traverseM ::
  (Monad m, Traversable m, Applicative f) =>
  (a1 -> f (m a2)) ->
  m a1 ->
  f (m a2)
traverseM f = fmap join . traverse f

forWith :: (Functor f) => f key -> (key -> val) -> f (key, val)
forWith = flip mapWith

forWithM :: (Traversable l, Applicative f) => l key -> (key -> f val) -> f (l (key, val))
forWithM = flip mapWithM

mapWith :: (Functor f) => (key -> val) -> f key -> f (key, val)
mapWith f = fmap (\x -> (x, f x))

mapWithM :: (Traversable l, Applicative f) => (key -> f val) -> l key -> f (l (key, val))
mapWithM f = traverse (\x -> (x,) <$> f x)

composeM :: (Monad m) => Int -> (a -> m a) -> a -> m a
composeM 0 _ a = return a
composeM n f a = composeM (n - 1) f a >>= f

compose :: Int -> (a -> a) -> a -> a
compose 0 _ a = a
compose n f a = f (compose (n - 1) f a)

--------------------------------------------------------------------------------
-- String related util functions.
--------------------------------------------------------------------------------

show :: (Show a, IsString str) => a -> str
show = fromString . Show.show

toUpperFirst :: String -> String
toUpperFirst [] = []
toUpperFirst (x : xs) = Char.toUpper x : xs

--------------------------------------------------------------------------------
-- Text
--------------------------------------------------------------------------------

prime :: Text -> Text
prime "" = "_X"
prime "?" = "_X"
prime name = case Text.splitOn "'" name of
  [name', ""] -> name' <> "'0"
  [name', num] -> name' <> "'" <> maybe (num <> "'") (show . (+ 1)) (Text.readMaybe (unpack num) :: Maybe Word)
  _ -> name <> "'"

divisibleBy :: (Integral a) => a -> a -> Bool
divisibleBy a b = a `mod` b == 0

freshName :: HashSet Text -> Text -> Text
freshName names name | HashSet.member name names = freshName names (prime name)
freshName _ name = name

isFirstLetter :: String -> Bool
isFirstLetter = \case
  h : _ -> isLetter h
  _ -> False

uniqueName :: (Show a) => Text -> a -> Text
uniqueName txt sym = txt <> "_" <> show sym

replaceSubtext :: [(Text, Text)] -> Text -> Text
replaceSubtext texts txt = foldr (uncurry Text.replace) txt texts

replaceText :: [(Text, Text)] -> Text -> Text
replaceText texts txt = fromMaybe txt (HashMap.lookup txt (HashMap.fromList texts))

--------------------------------------------------------------------------------
-- Foldable
--------------------------------------------------------------------------------

allSame :: forall t a. (Eq a, Foldable t) => t a -> Bool
allSame t = case nonEmpty t of
  Nothing -> True
  Just (a :| as) -> all (== a) as

nonEmpty :: (Foldable l) => l a -> Maybe (NonEmpty a)
nonEmpty = NonEmpty.nonEmpty . toList

foldr1 :: (a -> a -> a) -> NonEmpty a -> a
foldr1 = List.foldr1

sconcatMapM :: (Semigroup c, Monad m) => (a -> m c) -> NonEmpty a -> m c
sconcatMapM f = fmap sconcat . mapM f

sconcatMap :: (Semigroup c) => (a -> c) -> NonEmpty a -> c
sconcatMap f = sconcat . fmap f

mconcatMap :: (Monoid c, Foldable t) => (a -> c) -> t a -> c
mconcatMap f = List.mconcatMap f . toList

mconcatMapM :: (Monad m, Monoid c, Foldable t) => (a -> m c) -> t a -> m c
mconcatMapM f = Monad.mconcatMapM f . toList

concatWith :: (Foldable t, Monoid a) => (a -> a -> a) -> t a -> a
concatWith f ds = case nonEmpty ds of
  Nothing -> mempty
  Just ds' -> foldr1 f ds'
{-# INLINE concatWith #-}

-- | The length of the input list must be even
listByPairsExact :: forall a. [a] -> [(a, a)]
listByPairsExact = reverse . go []
  where
    go :: [(a, a)] -> [a] -> [(a, a)]
    go acc = \case
      [] -> acc
      [_] -> error "input list must have even length"
      x : y : ls -> go ((x, y) : acc) ls

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

combineMaps :: (Hashable k, Hashable v) => NonEmpty (HashMap k v) -> HashMap k v
combineMaps mps =
  HashMap.fromList
    . HashSet.toList
    . foldr (HashSet.intersection . HashSet.fromList) (HashSet.fromList mpv')
    $ mpvs'
  where
    mpv' :| mpvs' = fmap HashMap.toList mps

--------------------------------------------------------------------------------
-- List
--------------------------------------------------------------------------------

snocMaybe :: [a] -> Maybe a -> [a]
snocMaybe l = \case
  Nothing -> l
  Just x -> snoc l x

snocNonEmptyMaybe :: NonEmpty a -> Maybe a -> NonEmpty a
snocNonEmptyMaybe (h :| t) m = h :| snocMaybe t m

revAppend :: [a] -> [a] -> [a]
revAppend [] !ys = ys
revAppend (x : xs) !ys = revAppend xs (x : ys)

-- | strict map
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

findJustM :: forall a b m. (Monad m) => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
findJustM f = \case
  [] -> return Nothing
  x : xs -> f x >>= maybe (findJustM f xs) (return . Just)

-- | Returns the first element that returns Just and the list with the remaining elements
findJustAndRemove :: forall a b. (a -> Maybe b) -> [a] -> Maybe (b, [a])
findJustAndRemove p = go []
  where
    go :: [a] -> [a] -> Maybe (b, [a])
    go acc = \case
      [] -> Nothing
      x : xs -> case p x of
        Just b -> Just (b, reverse acc ++ xs)
        Nothing -> go (x : acc) xs

findAndRemove :: (a -> Bool) -> [a] -> Maybe (a, [a])
findAndRemove p = findJustAndRemove (\a -> guard (p a) $> a)

--------------------------------------------------------------------------------
-- NonEmpty
--------------------------------------------------------------------------------

nonEmptyUnsnoc :: NonEmpty a -> (Maybe (NonEmpty a), a)
nonEmptyUnsnoc e = (NonEmpty.nonEmpty (NonEmpty.init e), NonEmpty.last e)

tail' :: (HasCallStack) => [a] -> [a]
tail' = \case
  [] -> impossible
  _ : xs -> xs

head' :: (HasCallStack) => [a] -> a
head' = \case
  [] -> impossible
  x : _ -> x

nonEmpty' :: (HasCallStack) => [a] -> NonEmpty a
nonEmpty' = fromJust . nonEmpty

_nonEmpty :: Lens' [a] (Maybe (NonEmpty a))
_nonEmpty f x = maybe [] toList <$> f (nonEmpty x)

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

error :: (HasCallStack) => Text -> a
error = Err.error . unpack

{-# DEPRECATED todo "todo" #-}
todo :: (HasCallStack) => a
todo = Err.error "TODO"

{-# DEPRECATED undefined "undefined" #-}
undefined :: (HasCallStack) => a
undefined = Err.error "undefined"

-- | Used to indicate impossible corner cases.
impossible :: (HasCallStack) => a
impossible = Err.error "impossible"

impossibleError :: (HasCallStack) => Text -> a
impossibleError msg = Err.error ("impossible: " <> unpack msg)

--------------------------------------------------------------------------------

infixl 7 <+?>

(<+?>) :: Doc ann -> Maybe (Doc ann) -> Doc ann
(<+?>) a = maybe a (a <+>)

infixr 7 <?+>

(<?+>) :: Maybe (Doc ann) -> Doc ann -> Doc ann
(<?+>) = \case
  Nothing -> id
  Just a -> (a <+>)

infixr 7 ?<>?

(?<>?) :: (Semigroup m) => Maybe m -> Maybe m -> Maybe m
a ?<>? b = do
  a' <- a
  b' <- b
  return (a' <> b')

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
  deriving stock (Generic, Show, Eq, Ord, Foldable, Traversable)

instance Functor Indexed where
  fmap f (Indexed i a) = Indexed i (f a)

instance (Hashable a) => Hashable (Indexed a)

indexFrom :: Int -> [a] -> [Indexed a]
indexFrom i = zipWith Indexed [i ..]

makeLenses ''Indexed

getContents :: (MonadIO m) => m Text
getContents = liftIO Utf8.getContents

hClose :: (MonadIO m) => Handle -> m ()
hClose = liftIO . IO.hClose

hGetLine :: (MonadIO m) => Handle -> m Text
hGetLine = liftIO . Utf8.hGetLine

hIsEOF :: (MonadIO m) => Handle -> m Bool
hIsEOF = liftIO . IO.hIsEOF

hFlush :: (MonadIO m) => Handle -> m ()
hFlush = liftIO . IO.hFlush

toTuple :: Indexed a -> (Int, a)
toTuple i = (i ^. indexedIx, i ^. indexedThing)

filterIndexed :: (a -> Bool) -> [Indexed a] -> [Indexed a]
filterIndexed f = filter (f . (^. indexedThing))

fromText :: (IsString a) => Text -> a
fromText = fromString . unpack

fromRightIO' :: (MonadIO m) => (e -> m ()) -> m (Either e r) -> m r
fromRightIO' pp = do
  eitherM ifLeft return
  where
    ifLeft e = pp e >> exitFailure

fromRightIO :: (MonadIO m) => (e -> Text) -> m (Either e r) -> m r
fromRightIO pp = fromRightIO' (putStrLn . pp)

exitSuccess :: (MonadIO m) => m x
exitSuccess = liftIO IO.exitSuccess

exitFailure :: (MonadIO m) => m x
exitFailure = liftIO IO.exitFailure

print :: (MonadIO m, Show a) => a -> m ()
print = liftIO . IO.print

putStr :: (MonadIO m) => Text -> m ()
putStr = liftIO . Text.putStr

putStrLn :: (MonadIO m) => Text -> m ()
putStrLn = liftIO . Text.putStrLn

getLine :: (MonadIO m) => m Text
getLine = liftIO Text.getLine

hPutStr :: (MonadIO m) => Handle -> Text -> m ()
hPutStr h = liftIO . Text.hPutStr h

hPutStrLn :: (MonadIO m) => Handle -> Text -> m ()
hPutStrLn h = liftIO . Text.hPutStrLn h

optional_ :: (Alternative m) => m a -> m ()
optional_ = void . optional

--------------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------------

massert :: (HasCallStack, Applicative f) => Bool -> f ()
massert b = assert b (pure ())

-- | applies a function n times
iterateN :: Int -> (a -> a) -> a -> a
iterateN n f = (!! n) . iterate f

iterateNat :: Natural -> (a -> a) -> a -> a
iterateNat n f x
  | n == 0 = x
  | otherwise = f (iterateNat (n - 1) f x)

nubHashableNonEmpty :: (Hashable a) => NonEmpty a -> NonEmpty a
nubHashableNonEmpty = nonEmpty' . HashSet.toList . HashSet.fromList . toList

nubHashable :: (Hashable a) => [a] -> [a]
nubHashable = HashSet.toList . HashSet.fromList

allElements :: (Bounded a, Enum a) => [a]
allElements = [minBound .. maxBound]

replicate :: (Integral n) => n -> a -> [a]
replicate n a = List.replicate (fromIntegral n) a

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

-- | Valid Unix directory character that does not need to be escaped
isSafeDirectoryChar :: Char -> Bool
isSafeDirectoryChar =
  ((isLetter .||. isDigit) .&&. isAscii)
    .||. (`elem` ("_.-" :: [Char]))

isValidIdentChar :: Char -> Bool
isValidIdentChar = ((isLetter .||. isDigit) .&&. isAscii) .||. (`elem` ("_" :: [Char]))

class CanonicalProjection a b where
  project :: a -> b

instance CanonicalProjection a a where
  project = id

instance CanonicalProjection Void a where
  project = absurd

instance CanonicalProjection a () where
  project = const ()

instance Serialize Void

-- | 'project' with type arguments swapped. Useful for type application
project' :: forall b a. (CanonicalProjection a b) => a -> b
project' = project

ensureFile :: (MonadIO m, MonadThrow m) => Path Abs File -> m ()
ensureFile f =
  unlessM
    (Path.doesFileExist f)
    (throwM (mkIOError doesNotExistErrorType "" Nothing (Just (toFilePath f))))

flattenSCC :: SCC a -> NonEmpty a
flattenSCC = \case
  AcyclicSCC a -> pure a
  -- Ideally `CyclicSCC`'s argument would have type `NonEmpty a` instead of `[a]`
  CyclicSCC as -> nonEmpty' as

whileJustM :: forall m a. (Monad m) => m (Maybe a) -> m [a]
whileJustM m = go []
  where
    go :: [a] -> m [a]
    go acc = do
      r <- m
      case r of
        Nothing -> return (reverse acc)
        Just r' -> go (r' : acc)

sequenceEndWith :: (Monad m, Foldable l) => m () -> l (m ()) -> m ()
sequenceEndWith sep l = sequenceWith sep l >> sep

sequenceWith :: forall m l. (Monad m, Foldable l) => m () -> l (m ()) -> m ()
sequenceWith sep = go . toList
  where
    go :: [m ()] -> m ()
    go = \case
      [] -> return ()
      [x] -> x
      (x : xs) -> x >> sep >> go xs

-- | Removes the first element that satisfies a predicate and returns it
popFirstJust :: (a -> Maybe b) -> [a] -> (Maybe b, [a])
popFirstJust f = \case
  [] -> (Nothing, [])
  (h : hs) -> case f h of
    Nothing -> (h :) <$> popFirstJust f hs
    Just x -> (Just x, hs)

uncurryF :: (Functor f) => (a -> b -> c) -> f (a, b) -> f c
uncurryF g input_ = uncurry g <$> input_

intMapToList :: IntMap a -> [Indexed a]
intMapToList = map (uncurry Indexed) . IntMap.toList

intSet :: (Foldable f) => f (Int) -> IntSet
intSet = IntSet.fromList . toList

intMap :: (Foldable f) => f (Int, a) -> IntMap a
intMap = IntMap.fromList . toList

indexedByInt :: (Foldable f) => (a -> Int) -> f a -> IntMap a
indexedByInt getIx l = intMap [(getIx i, i) | i <- toList l]

indexedByHashList ::
  (Foldable f, Hashable k) =>
  (a -> k) ->
  f a ->
  HashMap k (NonEmpty a)
indexedByHashList getIx l = hashMapWith (<>) [(getIx i, pure i) | i <- toList l]

indexedByHash :: (Foldable f, Hashable k) => (a -> k) -> f a -> HashMap k a
indexedByHash getIx l = hashMap [(getIx i, i) | i <- toList l]

ordSet :: (Foldable f, Ord k) => f k -> Set k
ordSet = Set.fromList . toList

hashSet :: (Foldable f, Hashable k) => f k -> HashSet k
hashSet = HashSet.fromList . toList

hashMapFromHashSetM :: (Monad m, Hashable k) => HashSet k -> (k -> m v) -> m (HashMap k v)
hashMapFromHashSetM s fun =
  hashMap
    <$> sequence
      [ do
          r <- fun x
          return (x, r)
        | x <- toList s
      ]

hashMapFromHashSet :: (Hashable k) => HashSet k -> (k -> v) -> HashMap k v
hashMapFromHashSet s fun = hashMap [(x, fun x) | x <- toList s]

-- | Sorts and removes duplicates
ordNubSort :: (Foldable f, Ord k) => f k -> [k]
ordNubSort = toList . ordSet

ordMap :: (Foldable f, Ord k) => f (k, v) -> Map k v
ordMap = Map.fromList . toList

hashMapWith :: (Foldable f, Hashable k) => (v -> v -> v) -> f (k, v) -> HashMap k v
hashMapWith aggr = HashMap.fromListWith aggr . toList

hashMap :: (Foldable f, Hashable k) => f (k, v) -> HashMap k v
hashMap = HashMap.fromList . toList

hashMapInsertWeak :: (Hashable k) => k -> v -> HashMap k v -> HashMap k v
hashMapInsertWeak = HashMap.insertWith (\_new old -> old)

lazyHashMap :: (Foldable f, Hashable k) => f (k, v) -> LazyHashMap k v
lazyHashMap = LazyHashMap.fromList . toList

ensureLn :: Text -> Text
ensureLn t =
  case Text.unsnoc t of
    Nothing -> t
    Just (_, y) -> case y of
      '\n' -> t
      _ -> Text.snoc t '\n'

toFilePath :: (IsString s) => Path a b -> s
toFilePath = fromString . PPath.toFilePath

joinFilePaths :: (Foldable l) => l FilePath -> FilePath
joinFilePaths = FilePath.joinPath . toList

readFile :: (MonadIO m) => Path Abs File -> m Text
readFile = liftIO . Utf8.readFile . toFilePath

writeFileEnsureLn :: (MonadIO m) => Path Abs File -> Text -> m ()
writeFileEnsureLn p = liftIO . Utf8.writeFile (toFilePath p)
{-# INLINE writeFileEnsureLn #-}

-- | [a, b, c] -> [(a, b), (b, c), (c, a)]
zipWithNextLoop :: forall a. NonEmpty a -> NonEmpty (a, a)
zipWithNextLoop l = NonEmpty.reverse (go [] l)
  where
    h = head l

    go :: [(a, a)] -> NonEmpty a -> NonEmpty (a, a)
    go acc = \case
      lastA :| [] -> (lastA, h) :| acc
      x :| y : as -> go ((x, y) : acc) (y :| as)

mappendField :: (Semigroup f) => t -> t -> Lens' t f -> f
mappendField t1 t2 = appendFieldWith t1 t2 (<>)

appendFieldWith :: t -> t -> (f -> f -> f) -> Lens' t f -> f
appendFieldWith t1 t2 joinfun l = joinfun (t1 ^. l) (t2 ^. l)

unicodeSubscript :: Natural -> Text
unicodeSubscript = pack . map toSubscript . show
  where
    toSubscript :: Char -> Char
    toSubscript = \case
      '0' -> '₀'
      '1' -> '₁'
      '2' -> '₂'
      '3' -> '₃'
      '4' -> '₄'
      '5' -> '₅'
      '6' -> '₆'
      '7' -> '₇'
      '8' -> '₈'
      '9' -> '₉'
      _ -> impossible

-- | A list of vertices [v1, .., vn], s.t. ∀i, ⟨vi, v(i+1 `mod` n)⟩ ∈ Edges
newtype GraphCycle a = GraphCycle
  { _graphCycleVertices :: NonEmpty a
  }
  deriving stock (Show)

makeLenses ''GraphCycle

data GraphInfo node key = GraphInfo
  { _graphInfoGraph :: Graph,
    _graphInfoNodeFromVertex :: Vertex -> (node, key, [key]),
    _graphInfoKeyToVertex :: key -> Maybe Vertex
  }

makeLenses ''GraphInfo

mkGraphInfo :: (Ord key) => [(node, key, [key])] -> GraphInfo node key
mkGraphInfo e =
  let (_graphInfoGraph, _graphInfoNodeFromVertex, _graphInfoKeyToVertex) = Graph.graphFromEdges e
   in GraphInfo {..}

graphCycle :: forall node key. GraphInfo node key -> Maybe (GraphCycle node)
graphCycle gi =
  case mapM_ findCycle sccs of
    Right {} -> Nothing
    Left cycl ->
      Just
        . over graphCycleVertices (fmap getNode)
        . GraphCycle
        . NonEmpty.reverse
        $ cycl
  where
    sccs :: [Tree Vertex] = scc g
    g :: Graph = gi ^. graphInfoGraph

    getNode :: Vertex -> node
    getNode v = fst3 ((gi ^. graphInfoNodeFromVertex) v)

    isEdge :: Vertex -> Vertex -> Bool
    isEdge v u = u `elem` (g Array.! v)

    findCycle :: Tree Vertex -> Either (NonEmpty Vertex) ()
    findCycle (Node root ch) = goChildren (pure root) ch
      where
        go :: NonEmpty Vertex -> Tree Vertex -> Either (NonEmpty Vertex) ()
        go path (Node n ns)
          | isEdge n root = Left (NonEmpty.cons n path)
          | otherwise = goChildren (NonEmpty.cons n path) ns

        goChildren :: NonEmpty Vertex -> [Tree Vertex] -> Either (NonEmpty Vertex) ()
        goChildren path = mapM_ (go path)

allNaturals :: Stream Natural
allNaturals = Stream.iterate succ 0

allWords :: Stream Text
allWords = pack . toList <$> allFiniteSequences ('a' :| ['b' .. 'z'])

-- | Returns all non-empty finite sequences
allFiniteSequences :: forall a. NonEmpty a -> Stream (NonEmpty a)
allFiniteSequences elems = build 0 []
  where
    build :: Integer -> [NonEmpty a] -> Stream (NonEmpty a)
    build n = \case
      [] -> build (succ n) (toList (ofLength (succ n)))
      s : ss -> Stream.Cons s (build n ss)
    ofLength :: Integer -> NonEmpty (NonEmpty a)
    ofLength n
      | n < 1 = impossible
      | n == 1 = pure <$> elems
      | otherwise = do
          seq <- ofLength (n - 1)
          e <- elems
          return (pure e <> seq)

instance Serialize Text where
  put txt = Serial.put (unpack txt)

  get = pack <$> Serial.get

instance (Serialize a) => Serialize (NonEmpty a)

instance Serialize Versions.Chunk

instance Serialize Versions.Release

instance Serialize SemVer
