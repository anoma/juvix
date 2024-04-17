module Juvix.Compiler.Backend.Markdown.Data.MkJuvixBlockOptions where

import Juvix.Parser.Error.Base
import Juvix.Prelude.Base
import Juvix.Prelude.Parsing hiding (runParser)
import Juvix.Prelude.Path
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

data MkJuvixBlockOptions
  = MkJuvixBlockOptionsHide
  | MkJuvixBlockOptionsShow
  | MkJuvixBlockOptionsExtractModule
  deriving stock (Eq, Ord, Bounded, Enum, Show)

optionExtractModuleStatements :: (IsString s) => s
optionExtractModuleStatements = "extract-module-statements"

optionHide :: (IsString s) => s
optionHide = "hide"

renderJuvixBlockOptions :: MkJuvixBlockOptions -> Text
renderJuvixBlockOptions = \case
  MkJuvixBlockOptionsHide -> optionHide
  MkJuvixBlockOptionsShow -> ""
  MkJuvixBlockOptionsExtractModule -> optionExtractModuleStatements

parseJuvixBlockOptions :: Path Abs File -> Text -> Either MegaparsecError MkJuvixBlockOptions
parseJuvixBlockOptions p = mapLeft MegaparsecError . P.runParser parseOptions (toFilePath p)

type Parser = P.Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser ()
symbol = void . L.symbol spaceConsumer

parseOptions :: Parser MkJuvixBlockOptions
parseOptions = spaceConsumer >> parseHide <|> parseExtractModule <|> parseShow

parseShow :: Parser MkJuvixBlockOptions
parseShow = eof $> MkJuvixBlockOptionsShow

parseHide :: Parser MkJuvixBlockOptions
parseHide = symbol optionHide $> MkJuvixBlockOptionsHide <* lexeme eof

parseExtractModule :: Parser MkJuvixBlockOptions
parseExtractModule =
  symbol optionExtractModuleStatements
    $> MkJuvixBlockOptionsExtractModule
    <* lexeme eof
