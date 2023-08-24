module Juvix.Compiler.Concrete.Data.ParsedInfoTableBuilder
  ( module Juvix.Compiler.Concrete.Data.ParsedInfoTableBuilder,
    BuilderState,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Concrete.Data.Highlight.Input
import Juvix.Compiler.Concrete.Data.Literal
import Juvix.Compiler.Concrete.Data.ParsedInfoTable
import Juvix.Compiler.Concrete.Data.ParsedInfoTableBuilder.BuilderState
import Juvix.Compiler.Concrete.Language
import Juvix.Prelude

data InfoTableBuilder m a where
  RegisterItem :: ParsedItem -> InfoTableBuilder m ()
  RegisterSpaceSpan :: SpaceSpan -> InfoTableBuilder m ()
  RegisterModule :: Module 'Parsed 'ModuleTop -> InfoTableBuilder m ()
  VisitModule :: TopModulePath -> InfoTableBuilder m ()
  ModuleVisited :: TopModulePath -> InfoTableBuilder m Bool

makeSem ''InfoTableBuilder

registerKeyword :: (Member InfoTableBuilder r) => KeywordRef -> Sem r KeywordRef
registerKeyword r =
  r
    <$ registerItem
      ParsedItem
        { _parsedLoc = getLoc r,
          _parsedTag = ann
        }
  where
    ann = case r ^. keywordRefKeyword . keywordType of
      KeywordTypeKeyword -> ParsedTagKeyword
      KeywordTypeJudoc -> ParsedTagJudoc
      KeywordTypeDelimiter -> ParsedTagDelimiter

registerDelimiter :: (Member InfoTableBuilder r) => Interval -> Sem r ()
registerDelimiter i =
  registerItem
    ParsedItem
      { _parsedLoc = i,
        _parsedTag = ParsedTagDelimiter
      }

registerJudocText :: (Member InfoTableBuilder r) => Interval -> Sem r ()
registerJudocText i =
  registerItem
    ParsedItem
      { _parsedLoc = i,
        _parsedTag = ParsedTagJudoc
      }

registerPragmas :: (Member InfoTableBuilder r) => Interval -> Sem r ()
registerPragmas i =
  registerItem
    ParsedItem
      { _parsedLoc = i,
        _parsedTag = ParsedTagPragma
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

build :: BuilderState -> InfoTable
build st =
  InfoTable
    { _infoParsedComments = mkComments (st ^. stateComments),
      _infoParsedModules = st ^. stateModules
    }

registerItem' :: (Members '[HighlightBuilder] r) => ParsedItem -> Sem r ()
registerItem' i = modify' (over highlightParsed (i :))

runParserInfoTableBuilderRepl :: BuilderState -> Sem (InfoTableBuilder ': r) a -> Sem r (BuilderState, a)
runParserInfoTableBuilderRepl st = ignoreHighlightBuilder . runParserInfoTableBuilder' st . raiseUnder

runParserInfoTableBuilder' :: (Members '[HighlightBuilder] r) => BuilderState -> Sem (InfoTableBuilder ': r) a -> Sem r (BuilderState, a)
runParserInfoTableBuilder' s =
  runState s
    . reinterpret
      ( \case
          ModuleVisited i -> HashSet.member i <$> gets (^. stateVisited)
          VisitModule i -> modify' (over stateVisited (HashSet.insert i))
          RegisterModule m ->
            modify' (over stateModules (HashMap.insert (m ^. modulePath) m))
          RegisterItem i -> registerItem' i
          RegisterSpaceSpan g -> do
            modify' (over stateComments (g :))
            forM_ (g ^.. spaceSpan . each . _SpaceComment) $ \c ->
              registerItem'
                ParsedItem
                  { _parsedLoc = getLoc c,
                    _parsedTag = ParsedTagComment
                  }
      )

runParserInfoTableBuilder :: (Members '[HighlightBuilder] r) => Sem (InfoTableBuilder ': r) a -> Sem r (BuilderState, InfoTable, a)
runParserInfoTableBuilder m = do
  (builderState, x) <- runParserInfoTableBuilder' iniState m
  return (builderState, build builderState, x)
