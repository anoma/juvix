module Juvix.Compiler.Concrete.Data.ParsedInfoTableBuilder where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Concrete.Data.Literal
import Juvix.Compiler.Concrete.Data.ParsedInfoTable
import Juvix.Compiler.Concrete.Data.ParsedItem
import Juvix.Compiler.Concrete.Language
import Juvix.Prelude

data InfoTableBuilder m a where
  RegisterItem :: ParsedItem -> InfoTableBuilder m ()
  RegisterSpaceSpan :: SpaceSpan -> InfoTableBuilder m ()
  RegisterModule :: Module 'Parsed 'ModuleTop -> InfoTableBuilder m ()
  VisitModule :: TopModulePath -> InfoTableBuilder m ()
  ModuleVisited :: TopModulePath -> InfoTableBuilder m Bool

makeSem ''InfoTableBuilder

registerKeyword :: Member InfoTableBuilder r => KeywordRef -> Sem r KeywordRef
registerKeyword r =
  r
    <$ registerItem
      ParsedItem
        { _parsedLoc = getLoc r,
          _parsedTag = ParsedTagKeyword
        }

registerDelimiter :: Member InfoTableBuilder r => Interval -> Sem r ()
registerDelimiter i =
  registerItem
    ParsedItem
      { _parsedLoc = i,
        _parsedTag = ParsedTagDelimiter
      }

registerJudocText :: Member InfoTableBuilder r => Interval -> Sem r ()
registerJudocText i =
  registerItem
    ParsedItem
      { _parsedLoc = i,
        _parsedTag = ParsedTagJudoc
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
    _stateComments :: [SpaceSpan],
    _stateVisited :: HashSet TopModulePath,
    _stateModules :: HashMap TopModulePath (Module 'Parsed 'ModuleTop)
  }
  deriving stock (Show)

makeLenses ''BuilderState

iniState :: BuilderState
iniState =
  BuilderState
    { _stateItems = [],
      _stateComments = [],
      _stateVisited = mempty,
      _stateModules = mempty
    }

build :: BuilderState -> InfoTable
build st =
  InfoTable
    { _infoParsedItems = nubHashable (st ^. stateItems),
      _infoParsedComments = mkComments (st ^. stateComments),
      _infoParsedModules = st ^. stateModules
    }

registerItem' :: Members '[State BuilderState] r => ParsedItem -> Sem r ()
registerItem' i = modify' (over stateItems (i :))

runParserInfoTableBuilder' :: BuilderState -> Sem (InfoTableBuilder ': r) a -> Sem r (BuilderState, a)
runParserInfoTableBuilder' s =
  runState s
    . reinterpret
      ( \case
          ModuleVisited i -> HashSet.member i <$> gets (^. stateVisited)
          VisitModule i -> modify' (over stateVisited (HashSet.insert i))
          RegisterModule m ->
            modify' (over stateModules (HashMap.insert (m ^. modulePath) m))
          RegisterItem i ->
            modify' (over stateItems (i :))
          RegisterSpaceSpan g -> do
            modify' (over stateComments (g :))
            forM_ (g ^.. spaceSpan . each . _SpaceComment) $ \c ->
              registerItem'
                ParsedItem
                  { _parsedLoc = getLoc c,
                    _parsedTag = ParsedTagComment
                  }
      )

runParserInfoTableBuilder :: Sem (InfoTableBuilder ': r) a -> Sem r (BuilderState, InfoTable, a)
runParserInfoTableBuilder m = do
  (builderState, x) <- runParserInfoTableBuilder' iniState m
  return (builderState, build builderState, x)
