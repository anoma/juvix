module Juvix.Compiler.Concrete.Data.ParsedInfoTableBuilder
  ( InfoTableBuilder,
    registerLiteral,
    registerKeyword,
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
import Juvix.Prelude

data InfoTableBuilder m a where
  RegisterItem :: ParsedItem -> InfoTableBuilder m ()
  MergeTable :: InfoTable -> InfoTableBuilder m ()

makeSem ''InfoTableBuilder

registerComment :: (Member InfoTableBuilder r) => Interval -> Sem r ()
registerComment i =
  registerItem
    ParsedItem
      { _parsedLoc = i,
        _parsedTag = ParsedTagComment
      }

registerKeyword :: (Member InfoTableBuilder r) => Interval -> Sem r ()
registerKeyword i =
  registerItem
    ParsedItem
      { _parsedLoc = i,
        _parsedTag = ParsedTagKeyword
      }

registerLiteral :: (Member InfoTableBuilder r) => LiteralLoc -> Sem r LiteralLoc
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

newtype BuilderState = BuilderState
  { _stateItems :: [ParsedItem]
  }
  deriving stock (Show)

makeLenses ''BuilderState

iniState :: BuilderState
iniState =
  BuilderState
    { _stateItems = []
    }

build :: BuilderState -> InfoTable
build st = InfoTable (nubHashable (st ^. stateItems))

runInfoTableBuilder :: Sem (InfoTableBuilder ': r) a -> Sem r (InfoTable, a)
runInfoTableBuilder =
  fmap (first build)
    . runState iniState
    . reinterpret
      ( \case
          RegisterItem i ->
            modify' (over stateItems (i :))
          MergeTable tbl ->
            modify' (over stateItems ((tbl ^. infoParsedItems) <>))
      )

ignoreInfoTableBuilder :: Sem (InfoTableBuilder ': r) a -> Sem r a
ignoreInfoTableBuilder = fmap snd . runInfoTableBuilder
