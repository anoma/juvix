module Juvix.Compiler.Core.Data.TransformationId.Parser (parseTransformations, TransformationId (..)) where

import Juvix.Compiler.Core.Data.TransformationId
import Juvix.Prelude
import Juvix.Prelude.Pretty hiding (comma)
import Text.Megaparsec
import Text.Megaparsec.Char qualified as L
import Text.Megaparsec.Char.Lexer qualified as L

parseTransformations :: Text -> Either Text [TransformationId]
parseTransformations t = case runParser transformations "<input>" t of
  Left (err :: ParseErrorBundle Text Void) -> Left (prettyText (errorBundlePretty err))
  Right r -> return r

transformations :: MonadParsec e Text m => m [TransformationId]
transformations = do
  L.hspace
  sepEndBy transformation comma <* eof

lexeme :: MonadParsec e Text m => m a -> m a
lexeme = L.lexeme L.hspace

comma :: MonadParsec e Text m => m ()
comma = symbol ","

symbol :: MonadParsec e Text m => Text -> m ()
symbol = void . lexeme . chunk

transformation :: MonadParsec e Text m => m TransformationId
transformation =
  symbol "lifting" $> LambdaLifting
    <|> symbol "eta" $> TopEtaExpand
    <|> symbol "identity" $> Identity
