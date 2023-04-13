module Juvix.Compiler.Pragmas where

import Data.Aeson.BetterErrors
import Data.Yaml
import Juvix.Prelude hiding ((<|>))

data PragmaInline
  = InlineNever
  | InlineFullyApplied
  | InlinePartiallyApplied {_pragmaInlineArgsNum :: Int}
  deriving stock (Show, Eq, Ord)

newtype PragmaUnroll = PragmaUnroll
  { _pragmaUnrollDepth :: Int
  }
  deriving stock (Show, Eq, Ord)

data Pragmas = Pragmas
  { -- We keep the exact source of the pragma text. This is necessary, because
    -- pragmas are supposed to be backwards-compatible. Unrecognised pragmas
    -- should be ignored, but they still need to be printed out when
    -- pretty-printing. Also, we probably don't want to impose pragma formatting
    -- choices on the user.
    _pragmasSource :: ByteString,
    _pragmasInline :: Maybe PragmaInline,
    _pragmasUnroll :: Maybe PragmaUnroll
  }
  deriving stock (Show, Eq, Ord)

makeLenses ''PragmaUnroll
makeLenses ''Pragmas

type PragmaError = Text

instance FromJSON Pragmas where
  parseJSON = toAesonParser id parsePragmas
    where
      parsePragmas :: Parse PragmaError Pragmas
      parsePragmas = do
        let _pragmasSource :: ByteString
            _pragmasSource = ""
        _pragmasInline <- keyMay "inline" parseInline
        _pragmasUnroll <- keyMay "unroll" parseUnroll
        return Pragmas {..}

      parseInline :: Parse PragmaError PragmaInline
      parseInline = parseInlineArgsNum <|> parseInlineBool
        where
          parseInlineArgsNum :: Parse PragmaError PragmaInline
          parseInlineArgsNum = do
            _pragmaInlineArgsNum <- asIntegral
            return $ InlinePartiallyApplied {..}

          parseInlineBool :: Parse PragmaError PragmaInline
          parseInlineBool = do
            b <- asBool
            if
                | b -> return InlineFullyApplied
                | otherwise -> return InlineNever

      parseUnroll :: Parse PragmaError PragmaUnroll
      parseUnroll = do
        _pragmaUnrollDepth <- asIntegral
        return PragmaUnroll {..}
