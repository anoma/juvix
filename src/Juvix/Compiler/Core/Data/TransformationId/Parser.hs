module Juvix.Compiler.Core.Data.TransformationId.Parser (parseTransformations, TransformationId (..), completions, completionsString) where

import Data.Text qualified as Text
import Juvix.Compiler.Core.Data.TransformationId
import Juvix.Prelude
import Juvix.Prelude.Pretty hiding (comma)
import Text.Megaparsec as P
import Text.Megaparsec.Char qualified as L
import Text.Megaparsec.Char.Lexer qualified as L

parseHelper :: Parsec Void Text a -> Text -> Either Text a
parseHelper p t = case runParser p "<input>" t of
  Left (err :: ParseErrorBundle Text Void) -> Left (prettyText (errorBundlePretty err))
  Right r -> return r

parseTransformations :: Text -> Either Text [TransformationId]
parseTransformations = parseHelper transformations

completionsString :: String -> [String]
completionsString = map unpack . completions . pack

completions :: Text -> [Text]
completions = fromRight [] . parseHelper pcompletions

transformations :: MonadParsec e Text m => m [TransformationId]
transformations = do
  L.hspace
  sepEndBy transformation comma <* eof

-- | returns a possible list of completions
pcompletions :: MonadParsec e Text m => m [Text]
pcompletions = do
  L.hspace
  l <- sepEndBy transformation comma
  rest <- Text.strip <$> takeRest
  return [ppTransL (notNull l) l <> str | str <- allStrings, Text.isPrefixOf rest str]
  where
    ppTransL :: Bool -> [TransformationId] -> Text
    ppTransL c =
      let f :: Text -> Text = if c then (<> ",") else id
       in f . Text.intercalate "," . map ppTrans
    ppTrans :: TransformationId -> Text
    ppTrans = \case
      LambdaLifting -> strLifting
      TopEtaExpand -> strTopEtaExpand
      Identity -> strIdentity
      RemoveTypeArgs -> strRemoveTypeArgs

lexeme :: MonadParsec e Text m => m a -> m a
lexeme = L.lexeme L.hspace

comma :: MonadParsec e Text m => m ()
comma = symbol ","

symbol :: MonadParsec e Text m => Text -> m ()
symbol = void . lexeme . chunk

transformation :: MonadParsec e Text m => m TransformationId
transformation =
  symbol strLifting $> LambdaLifting
    <|> symbol strIdentity $> Identity
    <|> symbol strTopEtaExpand $> TopEtaExpand
    <|> symbol strRemoveTypeArgs $> RemoveTypeArgs

allStrings :: [Text]
allStrings =
  [ strLifting,
    strTopEtaExpand,
    strIdentity,
    strRemoveTypeArgs
  ]

strLifting :: Text
strLifting = "lifting"

strTopEtaExpand :: Text
strTopEtaExpand = "top-eta-expand"

strIdentity :: Text
strIdentity = "identity"

strRemoveTypeArgs :: Text
strRemoveTypeArgs = "remove-type-args"
