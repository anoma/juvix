module Juvix.Data.Yaml
  ( module Data.Aeson.BetterErrors,
    module Data.Yaml,
    module Juvix.Data.Yaml,
  )
where

import Data.Aeson.BetterErrors hiding (mapError, (<|>))
import Data.Aeson.Types (formatError)
import Data.Yaml (FromJSON (..), ParseException (..), prettyPrintParseException)
import Data.Yaml.Internal (Warning (..), decodeHelper)
import GHC.IO (unsafePerformIO)
import Juvix.Prelude.Base
import Text.Libyaml qualified as Y

type YamlError = Text

-- | Check that all keys are in the given list.
checkYamlKeys :: [Text] -> Parse YamlError ()
checkYamlKeys keys = do
  forEachInObject
    ( \k ->
        unless (k `elem` keys) $
          throwCustomError ("unknown key: " <> k)
    )
  return ()

decodeEither :: FromJSON a => ByteString -> Either ParseException a
decodeEither =
  either Left splitEither
    . unsafePerformIO
    . decodeHelper
    . Y.decode
  where
    splitEither :: ([Warning], Either String a) -> Either ParseException a
    splitEither (_, Left s) = Left (AesonException s)
    splitEither (w : _, Right _) = Left (AesonException (prettyPrintWarning w))
    splitEither ([], Right x) = Right x

prettyPrintWarning :: Warning -> String
prettyPrintWarning = \case
  DuplicateKey a -> formatError a "duplicate key"
