module Juvix.Compiler.Concrete.Translation.FromSource.ParserResultBuilder where

import Juvix.Compiler.Concrete.Data.Highlight.Input
import Juvix.Compiler.Concrete.Data.Literal
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromSource.Data.ParserState
import Juvix.Prelude

data ParserResultBuilder :: Effect where
  RegisterItem :: ParsedItem -> ParserResultBuilder m ()
  RegisterSpaceSpan :: SpaceSpan -> ParserResultBuilder m ()
  RegisterImport :: Import 'Parsed -> ParserResultBuilder m ()

makeSem ''ParserResultBuilder

registerKeyword :: (Member ParserResultBuilder r) => KeywordRef -> Sem r KeywordRef
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

registerDelimiter :: (Member ParserResultBuilder r) => Interval -> Sem r ()
registerDelimiter i =
  registerItem
    ParsedItem
      { _parsedLoc = i,
        _parsedTag = ParsedTagDelimiter
      }

registerJudocText :: (Member ParserResultBuilder r) => Interval -> Sem r ()
registerJudocText i =
  registerItem
    ParsedItem
      { _parsedLoc = i,
        _parsedTag = ParsedTagJudoc
      }

registerPragmas :: (Member ParserResultBuilder r) => Interval -> Sem r ()
registerPragmas i =
  registerItem
    ParsedItem
      { _parsedLoc = i,
        _parsedTag = ParsedTagPragma
      }

registerLiteral :: (Member ParserResultBuilder r) => LiteralLoc -> Sem r LiteralLoc
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
      LitIntegerWithBase {} -> ParsedTagLiteralInt
    loc = getLoc l

registerItem' :: (Member (State ParserState) r) => ParsedItem -> Sem r ()
registerItem' i = modify' (over parserStateParsedItems (i :))

runParserResultBuilder :: (Member HighlightBuilder r) => ParserState -> Sem (ParserResultBuilder ': r) a -> Sem r (ParserState, a)
runParserResultBuilder s =
  reinterpret (runState s) $ \case
    RegisterImport i -> modify' (over parserStateImports (i :))
    RegisterItem i -> do
      modify' (over highlightParsed (i :))
      registerItem' i
    RegisterSpaceSpan g -> do
      modify' (over parserStateComments (g :))
      forM_ (g ^.. spaceSpan . each . _SpaceComment) $ \c ->
        registerItem'
          ParsedItem
            { _parsedLoc = getLoc c,
              _parsedTag = ParsedTagComment
            }
