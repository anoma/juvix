module Juvix.Compiler.Concrete.Translation.FromSource.Data.ParserState where

import Juvix.Compiler.Concrete.Data.ParsedItem
import Juvix.Compiler.Concrete.Language
import Juvix.Prelude

data ParserState = ParserState
  { _parserStateImports :: [TopModulePath],
    _parserStateComments :: [SpaceSpan],
    _parserStateParsedItems :: [ParsedItem]
  }

makeLenses ''ParserState

instance Semigroup ParserState where
  s1 <> s2 =
    ParserState
      { _parserStateImports = s1 ^. parserStateImports <> s2 ^. parserStateImports,
        _parserStateComments = s1 ^. parserStateComments <> s2 ^. parserStateComments,
        _parserStateParsedItems = s1 ^. parserStateParsedItems <> s2 ^. parserStateParsedItems
      }

instance Monoid ParserState where
  mempty =
    ParserState
      { _parserStateImports = mempty,
        _parserStateComments = mempty,
        _parserStateParsedItems = mempty
      }
