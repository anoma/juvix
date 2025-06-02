module Juvix.Compiler.Reg.Translation.FromSource.Lexer
  ( module Juvix.Compiler.Reg.Translation.FromSource.Lexer,
    module Juvix.Compiler.Tree.Translation.FromSource.Lexer.Base,
    module Juvix.Compiler.Reg.Keywords,
  )
where

import Juvix.Compiler.Reg.Keywords
import Juvix.Compiler.Tree.Translation.FromSource.Lexer.Base
import Juvix.Parser.Error.Base
import Juvix.Prelude

int :: (Member (Error SimpleParserError) r) => ParsecS r Int
int = (^. withLocParam) <$> number @SimpleParserError (-(2 ^ (31 :: Int))) (2 ^ (31 :: Int))

smallnat :: (Member (Error SimpleParserError) r) => ParsecS r Int
smallnat = (^. withLocParam) <$> number @SimpleParserError 0 256

identifier :: ParsecS r Text
identifier = lexeme bareIdentifier

identifierL :: ParsecS r (Text, Interval)
identifierL = lexemeInterval bareIdentifier

bareIdentifier :: ParsecS r Text
bareIdentifier = rawIdentifier' (`elem` specialSymbols) allKeywordStrings
