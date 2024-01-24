{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid restricted extensions" #-}
{-# HLINT ignore "Avoid restricted flags" #-}
module Juvix.Compiler.Tree.Translation.FromSource.Sig where

import Control.Monad.Trans.Class (lift)
import Juvix.Compiler.Tree.Data.InfoTableBuilder.Base
import Juvix.Compiler.Tree.Language.Base
import Juvix.Compiler.Tree.Translation.FromSource.Lexer.Base

type LocalNameMap = HashMap Text DirectRef

data LocalParams = LocalParams
  { _localParamsNameMap :: LocalNameMap,
    _localParamsTempIndex :: Index
  }

data ParserSig t e = ParserSig
  { _parserSigBareIdentifier :: forall r. ParsecS r Text,
    _parserSigParseCode :: forall r. (Members '[Reader (ParserSig t e), InfoTableBuilder' t e, State LocalParams] r) => ParsecS r t,
    _parserSigEmptyCode :: t,
    _parserSigEmptyExtra :: e
  }

makeLenses ''ParserSig
makeLenses ''LocalParams

identifier :: forall t e r. (Member (Reader (ParserSig t e)) r) => ParsecS r Text
identifier = do
  sig <- lift $ ask @(ParserSig t e)
  lexeme (sig ^. parserSigBareIdentifier)

identifierL :: forall t e r. (Member (Reader (ParserSig t e)) r) => ParsecS r (Text, Interval)
identifierL = do
  sig <- lift $ ask @(ParserSig t e)
  lexemeInterval (sig ^. parserSigBareIdentifier)

parseCode :: forall t e r. (Members '[Reader (ParserSig t e), InfoTableBuilder' t e, State LocalParams] r) => ParsecS r t
parseCode = do
  sig <- lift $ ask @(ParserSig t e)
  sig ^. parserSigParseCode

emptyCode :: forall t e r. (Member (Reader (ParserSig t e)) r) => Sem r t
emptyCode = do
  sig <- ask @(ParserSig t e)
  return $ sig ^. parserSigEmptyCode

emptyExtra :: forall t e r. (Member (Reader (ParserSig t e)) r) => Sem r e
emptyExtra = do
  sig <- ask @(ParserSig t e)
  return $ sig ^. parserSigEmptyExtra
