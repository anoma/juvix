{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid restricted extensions" #-}
{-# HLINT ignore "Avoid restricted flags" #-}
module Juvix.Compiler.Tree.Translation.FromSource.Sig where

import Control.Monad.Trans.Class (lift)
import Juvix.Compiler.Tree.Translation.FromSource.Lexer.Base
import Juvix.Prelude

data ParserSig t = ParserSig
  { _parserSigBareIdentifier :: forall r. ParsecS r Text,
    _parserSigParseCode :: forall r. ParsecS r t,
    _parserSigEmptyCode :: t
  }

makeLenses ''ParserSig

identifier :: forall t r. (Member (Reader (ParserSig t)) r) => ParsecS r Text
identifier = do
  sig <- lift $ ask @(ParserSig t)
  lexeme (sig ^. parserSigBareIdentifier)

identifierL :: forall t r. (Member (Reader (ParserSig t)) r) => ParsecS r (Text, Interval)
identifierL = do
  sig <- lift $ ask @(ParserSig t)
  lexemeInterval (sig ^. parserSigBareIdentifier)

parseCode :: forall t r. (Member (Reader (ParserSig t)) r) => ParsecS r t
parseCode = do
  sig <- lift $ ask @(ParserSig t)
  sig ^. parserSigParseCode

emptyCode :: forall t r. (Member (Reader (ParserSig t)) r) => Sem r t
emptyCode = do
  sig <- ask @(ParserSig t)
  return $ sig ^. parserSigEmptyCode
