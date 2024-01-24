module Juvix.Compiler.Asm.Translation.FromSource.Lexer
  ( module Juvix.Compiler.Asm.Translation.FromSource.Lexer,
    module Juvix.Compiler.Tree.Translation.FromSource.Lexer.Base,
    module Juvix.Compiler.Asm.Keywords,
  )
where

import Juvix.Compiler.Asm.Keywords
import Juvix.Compiler.Tree.Translation.FromSource.Lexer.Base
import Juvix.Prelude

identifier :: ParsecS r Text
identifier = lexeme bareIdentifier

identifierL :: ParsecS r (Text, Interval)
identifierL = lexemeInterval bareIdentifier

bareIdentifier :: ParsecS r Text
bareIdentifier = rawIdentifier' (`elem` specialSymbols) allKeywordStrings
