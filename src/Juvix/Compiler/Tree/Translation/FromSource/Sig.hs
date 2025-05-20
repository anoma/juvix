{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid restricted extensions" #-}
{-# HLINT ignore "Avoid restricted flags" #-}
module Juvix.Compiler.Tree.Translation.FromSource.Sig where

import Control.Monad.Trans.Class (lift)
import Juvix.Compiler.Tree.Data.InfoTableBuilder.Base
import Juvix.Compiler.Tree.Language.Base
import Juvix.Compiler.Tree.Translation.FromSource.Lexer.Base
import Juvix.Parser.Error.Base

type LocalNameMap d = HashMap Text d

data LocalParams' d = LocalParams
  { _localParamsNameMap :: LocalNameMap d,
    _localParamsTempIndex :: Index
  }

emptyLocalParams :: LocalParams' d
emptyLocalParams =
  LocalParams
    { _localParamsNameMap = mempty,
      _localParamsTempIndex = 0
    }

data ParserSig t e d = ParserSig
  { _parserSigBareIdentifier :: forall r. ParsecS r Text,
    _parserSigParseCode :: forall r. (Members '[Error SimpleParserError, Reader (ParserSig t e d), InfoTableBuilder' t e, State (LocalParams' d)] r) => ParsecS r t,
    _parserSigArgRef :: Index -> Maybe Text -> d,
    _parserSigEmptyCode :: t,
    _parserSigEmptyExtra :: e
  }

makeLenses ''ParserSig
makeLenses ''LocalParams'

identifier :: forall t e d r. (Member (Reader (ParserSig t e d)) r) => ParsecS r Text
identifier = do
  sig <- lift $ ask @(ParserSig t e d)
  lexeme (sig ^. parserSigBareIdentifier)

identifierL :: forall t e d r. (Member (Reader (ParserSig t e d)) r) => ParsecS r (Text, Interval)
identifierL = do
  sig <- lift $ ask @(ParserSig t e d)
  lexemeInterval (sig ^. parserSigBareIdentifier)

parseCode :: forall t e d r. (Members '[Error SimpleParserError, Reader (ParserSig t e d), InfoTableBuilder' t e, State (LocalParams' d)] r) => ParsecS r t
parseCode = do
  sig <- lift $ ask @(ParserSig t e d)
  sig ^. parserSigParseCode

emptyCode :: forall t e d r. (Member (Reader (ParserSig t e d)) r) => Sem r t
emptyCode = do
  sig <- ask @(ParserSig t e d)
  return $ sig ^. parserSigEmptyCode

emptyExtra :: forall t e d r. (Member (Reader (ParserSig t e d)) r) => Sem r e
emptyExtra = do
  sig <- ask @(ParserSig t e d)
  return $ sig ^. parserSigEmptyExtra
