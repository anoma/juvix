module Juvix.Compiler.Concrete.Data.ParsedInfoTableBuilder
  ( InfoTableBuilder,
    registerLiteral,
    registerKeyword,
    registerJudocText,
    registerComment,
    mergeTable,
    runInfoTableBuilder,
    ignoreInfoTableBuilder,
    module Juvix.Compiler.Concrete.Data.ParsedInfoTable,
  )
where

import Juvix.Compiler.Concrete.Data.Literal
import Juvix.Compiler.Concrete.Data.ParsedInfoTable
import Juvix.Compiler.Concrete.Data.ParsedItem
import Juvix.Data.Comment
import Juvix.Data.Keyword
import Juvix.Prelude

data InfoTableBuilder m a where
  RegisterItem :: ParsedItem -> InfoTableBuilder m ()
  RegisterComment :: Comment -> InfoTableBuilder m ()
  MergeTable :: InfoTable -> InfoTableBuilder m ()

makeSem ''InfoTableBuilder

registerKeyword :: Member InfoTableBuilder r => KeywordRef -> Sem r KeywordRef
registerKeyword r =
  r
    <$ registerItem
      ParsedItem
        { _parsedLoc = getLoc r,
          _parsedTag = ParsedTagKeyword
        }

registerJudocText :: Member InfoTableBuilder r => Interval -> Sem r ()
registerJudocText i =
  registerItem
    ParsedItem
      { _parsedLoc = i,
        _parsedTag = ParsedTagComment
      }

registerLiteral :: Member InfoTableBuilder r => LiteralLoc -> Sem r LiteralLoc
registerLiteral l =
  l
    <$ registerItem
      ParsedItem
        { _parsedLoc = loc,
          _parsedTag = tag
        }
  where
    tag = case l ^. withLocParam of
      LitString {} -> ParsedTagLiteralString
      LitInteger {} -> ParsedTagLiteralInt
    loc = getLoc l

data BuilderState = BuilderState
  { _stateItems :: [ParsedItem],
    _stateComments :: [Comment]
  }
  deriving stock (Show)

makeLenses ''BuilderState

iniState :: BuilderState
iniState =
  BuilderState
    { _stateItems = [],
      _stateComments = []
    }

build :: BuilderState -> InfoTable
build st =
  InfoTable
    { _infoParsedItems = nubHashable (st ^. stateItems),
      _infoParsedComments = mkComments (st ^. stateComments)
    }

registerItem' :: Members '[State BuilderState] r => ParsedItem -> Sem r ()
registerItem' i = modify' (over stateItems (i :))

runInfoTableBuilder :: Sem (InfoTableBuilder ': r) a -> Sem r (InfoTable, a)
runInfoTableBuilder =
  fmap (first build)
    . runState iniState
    . reinterpret
      ( \case
          RegisterItem i ->
            modify' (over stateItems (i :))
          MergeTable tbl -> do
            modify' (over stateItems ((tbl ^. infoParsedItems) <>))
            modify' (over stateComments (allComments (tbl ^. infoParsedComments) <>))
          RegisterComment c -> do
            modify' (over stateComments (c :))
            registerItem'
              ParsedItem
                { _parsedLoc = c ^. commentInterval,
                  _parsedTag = ParsedTagComment
                }
      )

ignoreInfoTableBuilder :: Sem (InfoTableBuilder ': r) a -> Sem r a
ignoreInfoTableBuilder = fmap snd . runInfoTableBuilder
