module MiniJuvix.Syntax.Concrete.Parser.InfoTableBuilder
  ( InfoTableBuilder,
    registerLiteral,
    registerKeyword,
    mergeTable,
    runInfoTableBuilder,
    module MiniJuvix.Syntax.Concrete.Parser.InfoTable,
  )
where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Language (LiteralLoc (..))
import MiniJuvix.Syntax.Concrete.Literal
import MiniJuvix.Syntax.Concrete.Loc
import MiniJuvix.Syntax.Concrete.Parser.InfoTable
import MiniJuvix.Syntax.Concrete.Parser.ParsedItem

data InfoTableBuilder m a where
  RegisterItem :: ParsedItem -> InfoTableBuilder m ()
  MergeTable :: InfoTable -> InfoTableBuilder m ()

makeSem ''InfoTableBuilder

registerKeyword :: Member InfoTableBuilder r => Interval -> Sem r ()
registerKeyword i =
  registerItem
    ParsedItem
      { _parsedLoc = i,
        _parsedTag = ParsedTagKeyword
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
    tag = case _literalLocLiteral l of
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
  fmap (first build) . runState iniState
    . reinterpret
      ( \case
          RegisterItem i ->
            modify' (over stateItems (i :))
          MergeTable tbl ->
            modify' (over stateItems ((tbl ^. infoParsedItems) <>))
      )
