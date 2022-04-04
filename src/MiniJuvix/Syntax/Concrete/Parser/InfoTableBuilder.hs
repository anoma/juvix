module MiniJuvix.Syntax.Concrete.Parser.InfoTableBuilder (
  InfoTableBuilder,
  registerLiteral,
  registerKeyword,
  ignoring,
  mergeTable,
  runInfoTableBuilder,
  module MiniJuvix.Syntax.Concrete.Parser.InfoTable,
 ) where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Parser.ParsedItem
import MiniJuvix.Syntax.Concrete.Parser.InfoTable
import MiniJuvix.Syntax.Concrete.Literal
import MiniJuvix.Syntax.Concrete.Loc
import MiniJuvix.Syntax.Concrete.Language (LiteralLoc(..))

data InfoTableBuilder m a where
  RegisterItem :: ParsedItem -> InfoTableBuilder m ()
  Ignoring :: m a -> InfoTableBuilder m a
  MergeTable :: InfoTable -> InfoTableBuilder m ()

makeSem ''InfoTableBuilder

registerKeyword :: Member (InfoTableBuilder) r => Interval -> Sem r ()
registerKeyword i = registerItem ParsedItem {
  _parsedLoc = i,
  _parsedTag = ParsedTagKeyword
  }

registerLiteral :: Member (InfoTableBuilder) r => LiteralLoc -> Sem r LiteralLoc
registerLiteral l =
  l <$ registerItem ParsedItem {
    _parsedLoc = loc,
    _parsedTag = tag
  }
  where
  tag = case _literalLocLiteral l of
    LitString {} -> ParsedTagLiteralString
    LitInteger {} -> ParsedTagLiteralInt
  loc = getLoc l

data BuilderState = BuilderState {
  _stateIgnoring :: Bool,
  _stateItems :: [ParsedItem]
  }
  deriving stock (Show)
makeLenses ''BuilderState

iniState :: BuilderState
iniState = BuilderState {
  _stateIgnoring = False,
  _stateItems = []
  }

build :: BuilderState -> InfoTable
build st = InfoTable (st ^. stateItems)

runInfoTableBuilder :: Sem (InfoTableBuilder ': r) a -> Sem r (InfoTable, a)
runInfoTableBuilder = fmap (first build) . runState iniState . reinterpretH
  (\case
      RegisterItem i -> do
        unlessM (gets (^. stateIgnoring))
          (modify' (over stateItems (i :)))
        pureT ()
      Ignoring m -> do
        s0 <- gets (^. stateIgnoring)
        modify (set stateIgnoring True)
        r <- runTSimple m
        modify (over stateIgnoring (const s0))
        return r
      MergeTable tbl -> do
        modify' (over stateItems ((tbl ^. infoParsedItems)  <>))
        pureT ()
  )
