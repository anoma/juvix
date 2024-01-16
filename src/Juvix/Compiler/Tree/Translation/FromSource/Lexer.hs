module Juvix.Compiler.Tree.Translation.FromSource.Lexer
  ( module Juvix.Compiler.Tree.Translation.FromSource.Lexer,
    module Juvix.Compiler.Tree.Translation.FromSource.Lexer.Base,
    module Juvix.Compiler.Tree.Keywords,
  )
where

import Juvix.Compiler.Tree.Keywords
import Juvix.Compiler.Tree.Translation.FromSource.Lexer.Base
import Juvix.Prelude

identifier :: ParsecS r Text
identifier = lexeme bareIdentifier

identifierL :: ParsecS r (Text, Interval)
identifierL = lexemeInterval bareIdentifier

bareIdentifier :: ParsecS r Text
bareIdentifier = rawIdentifier' (`elem` specialSymbols) allKeywordStrings
