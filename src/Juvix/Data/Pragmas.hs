module Juvix.Data.Pragmas where

import Data.Aeson.BetterErrors qualified as Aeson
import Juvix.Data.Yaml
import Juvix.Extra.Serialize
import Juvix.Prelude.Base

data PragmaInline
  = InlineAlways
  | InlineNever
  | InlineCase
  | InlineFullyApplied
  | InlinePartiallyApplied {_pragmaInlineArgsNum :: Int}
  deriving stock (Show, Eq, Ord, Data, Generic)

newtype PragmaUnroll = PragmaUnroll
  { _pragmaUnrollDepth :: Int
  }
  deriving stock (Show, Eq, Ord, Data, Generic)

newtype PragmaArgNames = PragmaArgNames
  { _pragmaArgNames :: [Text]
  }
  deriving stock (Show, Eq, Ord, Data, Generic)

newtype PragmaPublic = PragmaPublic
  { _pragmaPublic :: [Text]
  }
  deriving stock (Show, Eq, Ord, Data, Generic)

newtype PragmaFormat = PragmaFormat
  { _pragmaFormat :: Bool
  }
  deriving stock (Show, Eq, Ord, Data, Generic)

data PragmaSpecialiseArg
  = SpecialiseArgNum Int
  | SpecialiseArgNamed Text
  deriving stock (Show, Eq, Ord, Data, Generic)

newtype PragmaSpecialiseArgs = PragmaSpecialiseArgs
  { _pragmaSpecialiseArgs :: [PragmaSpecialiseArg]
  }
  deriving stock (Show, Eq, Ord, Data, Generic)

newtype PragmaSpecialise = PragmaSpecialise
  { _pragmaSpecialise :: Bool
  }
  deriving stock (Show, Eq, Ord, Data, Generic)

newtype PragmaSpecialiseBy = PragmaSpecialiseBy
  { _pragmaSpecialiseBy :: [Text]
  }
  deriving stock (Show, Eq, Ord, Data, Generic)

newtype PragmaEval = PragmaEval
  { _pragmaEval :: Bool
  }
  deriving stock (Show, Eq, Ord, Data, Generic)

data Pragmas = Pragmas
  { _pragmasInline :: Maybe PragmaInline,
    _pragmasUnroll :: Maybe PragmaUnroll,
    _pragmasArgNames :: Maybe PragmaArgNames,
    _pragmasPublic :: Maybe PragmaPublic,
    _pragmasFormat :: Maybe PragmaFormat,
    _pragmasSpecialise :: Maybe PragmaSpecialise,
    _pragmasSpecialiseArgs :: Maybe PragmaSpecialiseArgs,
    _pragmasSpecialiseBy :: Maybe PragmaSpecialiseBy,
    _pragmasEval :: Maybe PragmaEval
  }
  deriving stock (Show, Eq, Ord, Data, Generic)

makeLenses ''PragmaUnroll
makeLenses ''PragmaArgNames
makeLenses ''PragmaPublic
makeLenses ''PragmaFormat
makeLenses ''PragmaSpecialiseArgs
makeLenses ''PragmaSpecialiseBy
makeLenses ''PragmaEval
makeLenses ''Pragmas

instance Hashable PragmaInline

instance Hashable PragmaUnroll

instance Hashable PragmaArgNames

instance Hashable PragmaPublic

instance Hashable PragmaFormat

instance Hashable PragmaSpecialiseArg

instance Hashable PragmaSpecialiseArgs

instance Hashable PragmaSpecialise

instance Hashable PragmaSpecialiseBy

instance Hashable PragmaEval

instance Hashable Pragmas

instance Serialize PragmaInline

instance Serialize PragmaUnroll

instance Serialize PragmaArgNames

instance Serialize PragmaPublic

instance Serialize PragmaFormat

instance Serialize PragmaSpecialiseArg

instance Serialize PragmaSpecialiseArgs

instance Serialize PragmaSpecialise

instance Serialize PragmaSpecialiseBy

instance Serialize PragmaEval

instance Serialize Pragmas

instance FromJSON Pragmas where
  parseJSON = toAesonParser id parsePragmas
    where
      parsePragmas :: Parse YamlError Pragmas
      parsePragmas = do
        _pragmasInline <- keyMay "inline" parseInline
        _pragmasUnroll <- keyMay "unroll" parseUnroll
        _pragmasArgNames <- keyMay "argnames" parseArgNames
        _pragmasPublic <- keyMay "public" parsePublicArgs
        _pragmasFormat <- keyMay "format" parseFormat
        spec <- keyMay "specialise" parseSpecialise
        spec' <- keyMay "specialize" parseSpecialise
        let bspec = spec >>= either Just (const Nothing)
            bspec' = spec' >>= either Just (const Nothing)
            _pragmasSpecialise = bspec <|> bspec'
        let specargs0 = spec >>= either (const Nothing) Just
            specargs0' = spec' >>= either (const Nothing) Just
        specargs <- keyMay "specialise-args" parseSpecialiseArgs
        specargs' <- keyMay "specialize-args" parseSpecialiseArgs
        let _pragmasSpecialiseArgs = specargs0 <|> specargs0' <|> specargs <|> specargs'
        specby <- keyMay "specialise-by" parseSpecialiseBy
        specby' <- keyMay "specialize-by" parseSpecialiseBy
        let _pragmasSpecialiseBy = specby <|> specby'
        _pragmasEval <- keyMay "eval" parseEval
        return Pragmas {..}

      parseInline :: Parse YamlError PragmaInline
      parseInline = parseInlineArgsNum Aeson.<|> parseInlineBool Aeson.<|> parseInlineAlwaysNever
        where
          parseInlineArgsNum :: Parse YamlError PragmaInline
          parseInlineArgsNum = do
            _pragmaInlineArgsNum <- asIntegral
            return InlinePartiallyApplied {..}

          parseInlineBool :: Parse YamlError PragmaInline
          parseInlineBool = do
            b <- asBool
            (if b then return InlineFullyApplied else return InlineNever)

          parseInlineAlwaysNever :: Parse YamlError PragmaInline
          parseInlineAlwaysNever = do
            txt <- asText
            case txt of
              "always" -> return InlineAlways
              "never" -> return InlineNever
              "case" -> return InlineCase
              _ -> throwCustomError ("unrecognized inline specification: " <> txt)

      parseUnroll :: Parse YamlError PragmaUnroll
      parseUnroll = do
        _pragmaUnrollDepth <- asIntegral
        return PragmaUnroll {..}

      parseArgNames :: Parse YamlError PragmaArgNames
      parseArgNames = do
        _pragmaArgNames <- eachInArray asText
        mapM_ checkArgName _pragmaArgNames
        return PragmaArgNames {..}

      parsePublicArgs :: Parse YamlError PragmaPublic
      parsePublicArgs = do
        _pragmaPublic <- eachInArray asText
        mapM_ checkArgName _pragmaPublic
        return PragmaPublic {..}

      parseFormat :: Parse YamlError PragmaFormat
      parseFormat = do
        _pragmaFormat <- asBool
        return PragmaFormat {..}

      parseEval :: Parse YamlError PragmaEval
      parseEval = do
        _pragmaEval <- asBool
        return PragmaEval {..}

      parseSpecialiseArg :: Parse YamlError PragmaSpecialiseArg
      parseSpecialiseArg =
        (SpecialiseArgNum <$> asIntegral)
          Aeson.<|> (SpecialiseArgNamed <$> asText)

      parseSpecialiseArgs :: Parse YamlError PragmaSpecialiseArgs
      parseSpecialiseArgs = do
        _pragmaSpecialiseArgs <- eachInArray parseSpecialiseArg
        return PragmaSpecialiseArgs {..}

      parseSpecialise :: Parse YamlError (Either PragmaSpecialise PragmaSpecialiseArgs)
      parseSpecialise =
        (Left . PragmaSpecialise <$> asBool)
          Aeson.<|> (Right <$> parseSpecialiseArgs)

      parseSpecialiseBy :: Parse YamlError PragmaSpecialiseBy
      parseSpecialiseBy = do
        _pragmaSpecialiseBy <- eachInArray asText
        return PragmaSpecialiseBy {..}

      checkArgName :: Text -> Parse YamlError ()
      checkArgName name = do
        let name' = unpack name
        unless (isFirstLetter name' && all isValidIdentChar name') $
          throwCustomError ("invalid argument name: " <> name)

-- | The Semigroup `<>` is used to propagate pragmas from an enclosing context.
-- For example, if `p1` are the pragmas declared for a module `M`, and `p2` the
-- pragmas declared for a function `f` inside `M`, then the actual pragmas for
-- `f` are `p1 <> p2`.
instance Semigroup Pragmas where
  p1 <> p2 =
    Pragmas
      { _pragmasInline = p2 ^. pragmasInline <|> p1 ^. pragmasInline,
        _pragmasUnroll = p2 ^. pragmasUnroll <|> p1 ^. pragmasUnroll,
        _pragmasArgNames = p2 ^. pragmasArgNames,
        _pragmasPublic = p2 ^. pragmasPublic,
        _pragmasFormat = p2 ^. pragmasFormat <|> p1 ^. pragmasFormat,
        _pragmasEval = p2 ^. pragmasEval <|> p1 ^. pragmasEval,
        _pragmasSpecialise = p2 ^. pragmasSpecialise <|> p1 ^. pragmasSpecialise,
        _pragmasSpecialiseArgs = p2 ^. pragmasSpecialiseArgs <|> p1 ^. pragmasSpecialiseArgs,
        _pragmasSpecialiseBy = p2 ^. pragmasSpecialiseBy <|> p1 ^. pragmasSpecialiseBy
      }

instance Monoid Pragmas where
  mempty =
    Pragmas
      { _pragmasInline = Nothing,
        _pragmasUnroll = Nothing,
        _pragmasArgNames = Nothing,
        _pragmasPublic = Nothing,
        _pragmasFormat = Nothing,
        _pragmasSpecialise = Nothing,
        _pragmasSpecialiseArgs = Nothing,
        _pragmasSpecialiseBy = Nothing,
        _pragmasEval = Nothing
      }

adjustPragmaInline :: Int -> PragmaInline -> PragmaInline
adjustPragmaInline n = \case
  InlinePartiallyApplied k -> InlinePartiallyApplied (k + n)
  InlineAlways -> InlineAlways
  InlineNever -> InlineNever
  InlineCase -> InlineCase
  InlineFullyApplied -> InlineFullyApplied

adjustPragmaSpecialiseArg :: Int -> PragmaSpecialiseArg -> PragmaSpecialiseArg
adjustPragmaSpecialiseArg n = \case
  SpecialiseArgNum i -> SpecialiseArgNum (n + i)
  SpecialiseArgNamed txt -> SpecialiseArgNamed txt

adjustPragmas :: Int -> Pragmas -> Pragmas
adjustPragmas fvnum pragmas =
  over pragmasInline (fmap (adjustPragmaInline fvnum)) $
    over
      pragmasSpecialiseArgs
      (fmap (over pragmaSpecialiseArgs (map (adjustPragmaSpecialiseArg fvnum))))
      pragmas
