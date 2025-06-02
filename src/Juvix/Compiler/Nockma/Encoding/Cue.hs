module Juvix.Compiler.Nockma.Encoding.Cue where

import Data.Bit as Bit
import Data.ByteString.Base64 qualified as Base64
import Juvix.Compiler.Nockma.Encoding.Base
import Juvix.Compiler.Nockma.Encoding.ByteString
import Juvix.Compiler.Nockma.Encoding.Effect.BitReader
import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Pretty.Base
import Juvix.Data.Error.GenericError
import Juvix.Prelude.Base
import VectorBuilder.Builder as Builder
import VectorBuilder.Vector

newtype CueState a = CueState
  { _cueStateCache :: HashMap Int (Term a)
  }

initCueState :: CueState a
initCueState =
  CueState
    { _cueStateCache = mempty
    }

newtype CueEnv = CueEnv
  {_cueEnvStartPos :: Int}

initCueEnv :: CueEnv
initCueEnv = CueEnv {_cueEnvStartPos = 0}

makeLenses ''CueState
makeLenses ''CueEnv

data DecodingError
  = DecodingErrorInvalidTag
  | DecodingErrorCacheMiss
  | DecodingErrorInvalidLength
  | DecodingErrorExpectedAtom
  | DecodingErrorInvalidAtom
  | DecodingErrorInvalidBackref
  deriving stock (Show)

instance Pretty DecodingError where
  pretty = unAnnotate . ppCodeAnn

instance PrettyCodeAnn DecodingError where
  ppCodeAnn = \case
    DecodingErrorInvalidTag -> "Invalid tag"
    DecodingErrorCacheMiss -> "Cache miss"
    DecodingErrorInvalidLength -> "Invalid length"
    DecodingErrorExpectedAtom -> "Expected atom"
    DecodingErrorInvalidAtom -> "Invalid atom"
    DecodingErrorInvalidBackref -> "Invalid backref"

instance PrettyCode DecodingError where
  ppCode = return . ppCodeAnn

-- | Register the start of processing a new entity
registerElementStart ::
  ( Members
      '[ BitReader,
         Reader CueEnv
       ]
      r
  ) =>
  Sem r a ->
  Sem r a
registerElementStart sem = do
  pos <- getCurrentPosition
  local (set cueEnvStartPos pos) sem

-- | Convert a BitReadError to a DecodingError
handleBitError :: (Member (Error DecodingError) r) => DecodingError -> Sem (Error BitReadError ': r) x -> Sem r x
handleBitError e = mapError @BitReadError (const e)

-- | Consume the encoded length from the input bits
consumeLength' :: forall r. (Members '[BitReader, Error DecodingError] r) => Sem r Int
consumeLength' = handleBitError DecodingErrorInvalidLength consumeLength

-- | Consume a nock integer from the input bits
consumeInteger ::
  forall r.
  ( Members
      '[ BitReader,
         Error DecodingError
       ]
      r
  ) =>
  DecodingError ->
  Int ->
  Sem r Integer
consumeInteger e len
  | len == 0 = return 0
  | otherwise =
      vectorBitsToInteger
        . build
        . foldable
        <$> handleBitError e (nextBits len)

data JamTag = JamTagAtom | JamTagCell | JamTagBackref

-- | Consume a nock tag from the input bits
consumeTag ::
  forall r.
  ( Members
      '[ BitReader,
         Error DecodingError
       ]
      r
  ) =>
  Sem r JamTag
consumeTag = do
  Bit b0 <- nextBit'
  if
      | b0 -> do
          Bit b1 <- nextBit'
          if
              | b1 -> return JamTagBackref
              | otherwise -> return JamTagCell
      | otherwise -> return JamTagAtom
  where
    nextBit' :: Sem r Bit
    nextBit' = handleBitError DecodingErrorInvalidTag nextBit

cacheCueTerm ::
  forall a r.
  ( Members
      '[ State (CueState a),
         Reader CueEnv
       ]
      r
  ) =>
  Term a ->
  Sem r ()
cacheCueTerm t = do
  pos <- asks (^. cueEnvStartPos)
  modify' (set (cueStateCache . at pos) (Just t))

lookupCueCache ::
  ( Members
      '[ Error DecodingError,
         State (CueState a)
       ]
      r
  ) =>
  Int ->
  Sem r (Term a)
lookupCueCache pos =
  fromMaybeM
    (throw DecodingErrorCacheMiss)
    (gets (^. cueStateCache . at pos))

-- | Transform an atom to a vector of bits
atomToBits ::
  forall a r.
  ( NockNatural a,
    Member (Error (ErrNockNatural' a)) r
  ) =>
  Atom a ->
  Sem r (Bit.Vector Bit)
atomToBits a' = do
  n <- nockNatural' a'
  return (integerToVectorBits @Integer (fromIntegral n))

-- | Transform a vector of bits to a decoded term
cueFromBits ::
  forall a r.
  ( Hashable a,
    NockNatural a,
    Members
      '[ Error DecodingError,
         Error (ErrNockNatural' a)
       ]
      r
  ) =>
  Bit.Vector Bit ->
  Sem r (Term a)
cueFromBits v = evalBitReader v (evalState (initCueState @a) (runReader initCueEnv cueFromBitsSem))

cueFromByteStringNatural ::
  forall r.
  ( Members
      '[ Error DecodingError,
         Error NockNaturalNaturalError,
         Error (ErrNockNatural' Natural)
       ]
      r
  ) =>
  ByteString ->
  Sem r (Term Natural)
cueFromByteStringNatural = cueFromByteString'

cueFromByteString' ::
  forall a r.
  ( Hashable a,
    NockNatural a,
    Members
      '[ Error DecodingError,
         Error (ErrNockNatural' a)
       ]
      r
  ) =>
  ByteString ->
  Sem r (Term a)
cueFromByteString' = cueFromBits . cloneFromByteString

cueFromBitsSem ::
  forall a r.
  ( Hashable a,
    NockNatural a,
    Members
      '[ BitReader,
         Error DecodingError,
         State (CueState a),
         Reader CueEnv,
         Error (ErrNockNatural' a)
       ]
      r
  ) =>
  Sem r (Term a)
cueFromBitsSem = registerElementStart $ do
  tag <- consumeTag
  case tag of
    JamTagAtom -> goAtom
    JamTagBackref -> goBackref
    JamTagCell -> goCell
  where
    goAtom :: Sem r (Term a)
    goAtom = do
      a <- TermAtom <$> consumeAtom
      cacheCueTerm a
      return a

    goBackref :: Sem r (Term a)
    goBackref = do
      a <- consumeNatAtom
      idx <- maybe (throw DecodingErrorInvalidBackref) return (safeNaturalToInt (a ^. atom))
      lookupCueCache idx

    goCell :: Sem r (Term a)
    goCell = do
      l <- cueFromBitsSem
      r <- cueFromBitsSem
      let cell = TermCell (mkCell l r)
      cacheCueTerm cell
      return cell

    consumeNatAtom :: Sem r (Atom Natural)
    consumeNatAtom = do
      len <- consumeLength'
      if
          | len == 0 -> return (Atom 0 emptyAtomInfo)
          | otherwise -> do
              a <- consumeInteger DecodingErrorInvalidAtom len
              return (Atom (fromInteger a) emptyAtomInfo)

    consumeAtom :: Sem r (Atom a)
    consumeAtom = do
      len <- consumeLength'
      if
          | len == 0 -> do
              z <- fromNatural' @a 0
              return (Atom z emptyAtomInfo)
          | otherwise -> do
              a <- consumeInteger DecodingErrorInvalidAtom len
              n <- fromNatural' (fromInteger a)
              return (Atom n emptyAtomInfo)

-- | Decode an nock Atom to a nock term
cue ::
  forall a r.
  ( Hashable a,
    NockNatural a,
    Members
      '[ Error DecodingError,
         Error (ErrNockNatural a)
       ]
      r
  ) =>
  Atom a ->
  Sem r (Term a)
cue a' =
  runErrorNoCallStackWith @(ErrNockNatural' a)
    (\(ErrNockNatural' e) -> throw e)
    (cue' a')

-- | A variant of cue with `ErrNockNatural` wrapped in a newtype to disambiguate it from DecodingError
cue' ::
  forall a r.
  ( Hashable a,
    NockNatural a,
    Members
      '[ Error DecodingError,
         Error (ErrNockNatural' a)
       ]
      r
  ) =>
  Atom a ->
  Sem r (Term a)
cue' a' = atomToBits a' >>= cueFromBits

cueEither ::
  -- NB: The signature returns the DecodingError in an Either to avoid
  -- overlapping instances with `ErrNockNatural a` when errors are handled. See
  -- the comment above `ErrNockNatural' a` for more explanation.
  forall a r.
  ( Hashable a,
    NockNatural a,
    Member (Error (ErrNockNatural a)) r
  ) =>
  Atom a ->
  Sem r (Either DecodingError (Term a))
cueEither =
  runErrorNoCallStackWith @(ErrNockNatural' a) (\(ErrNockNatural' e) -> throw e)
    . runErrorNoCallStack @DecodingError
    . cue'

cueFromByteString ::
  -- NB: The signature returns the DecodingError in an Either to avoid
  -- overlapping instances with `ErrNockNatural a` when errors are handled. See
  -- the comment above `ErrNockNatural' a` for more explanation.
  forall a r.
  ( Hashable a,
    NockNatural a,
    Member (Error (ErrNockNatural a)) r
  ) =>
  ByteString ->
  Sem r (Either DecodingError (Term a))
cueFromByteString =
  runErrorNoCallStackWith @(ErrNockNatural' a) (\(ErrNockNatural' e) -> throw e)
    . runErrorNoCallStack @DecodingError
    . cueFromByteString'

cueFromByteString'' ::
  forall a.
  (Hashable a, NockNatural a) =>
  ByteString ->
  Either (ErrNockNatural a) (Either DecodingError (Term a))
cueFromByteString'' = run . runErrorNoCallStack . cueFromByteString

{- `ErrNockNatural a` must be wrapped in a newtype to avoid overlapping instances
with `DecodingError` when errors are handled before the type variable `a` is
resolved.

When handling an error with `runError` before `a` is resolved, the compiler
cannot distinguish between `Error (ErrNockNatural a)` and `Error DecodingError`.
For some `a` it's possible that `ErrNockNatural a` is equal to `DecodingError`.
-}
newtype ErrNockNatural' a = ErrNockNatural' {_unErrNocknatural' :: ErrNockNatural a}

fromNatural' :: forall a r. (NockNatural a, Member (Error (ErrNockNatural' a)) r) => Natural -> Sem r a
fromNatural' = mapError (ErrNockNatural' @a) . fromNatural

nockNatural' :: forall a r. (NockNatural a, Member (Error (ErrNockNatural' a)) r) => Atom a -> Sem r Natural
nockNatural' = mapError (ErrNockNatural' @a) . nockNatural

decodeCue :: (Members '[Error SimpleError] r) => ByteString -> Sem r (Term Natural)
decodeCue encoded =
  case cueFromByteString'' encoded of
    Left (err :: NockNaturalNaturalError) -> throw (simpleErrorCodeAnn err)
    Right (Left (err :: DecodingError)) -> throw (simpleErrorCodeAnn err)
    Right (Right r) -> return r

decodeCue64 :: (Members '[Error SimpleError] r) => Text -> Sem r (Term Natural)
decodeCue64 = decode64 >=> decodeCue

decode64 :: (Members '[Error SimpleError] r) => Text -> Sem r ByteString
decode64 encoded =
  case Base64.decode (encodeUtf8 encoded) of
    Left err -> throw (SimpleError (mkAnsiText err))
    Right bs' -> return bs'
