module Juvix.Compiler.Backend.Markdown.Data.MkJuvixBlockOptions where

import Juvix.Parser.Error.Base
import Juvix.Prelude.Base
import Juvix.Prelude.Parsing hiding (runParser)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

data MkJuvixBlockOptions
  = MkJuvixBlockOptionsHide
  | MkJuvixBlockOptionsShow
  | MkJuvixBlockOptionsExtractModule JuvixBlockOptionsExtractModule
  deriving stock (Eq, Show)

newtype JuvixBlockOptionsExtractModule = JuvixBlockOptionsExtractModule
  { _juvixBlockOptionsExtractModuleDrop :: Int
  }
  deriving stock (Eq, Show)

makeLenses ''JuvixBlockOptionsExtractModule

optionExtractModuleStatements :: (IsString s) => s
optionExtractModuleStatements = "extract-module-statements"

optionHide :: (IsString s) => s
optionHide = "hide"

parseJuvixBlockOptions :: Path Abs File -> Text -> Either MegaparsecError MkJuvixBlockOptions
parseJuvixBlockOptions p = mapLeft MegaparsecError . P.runParser parseOptions (toFilePath p)

type Parser = P.Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser ()
symbol = void . L.symbol spaceConsumer

decimal :: Parser Int
decimal = lexeme L.decimal

parseOptions :: Parser MkJuvixBlockOptions
parseOptions = do
  spaceConsumer
  opts <-
    parseHide
      <|> parseExtractModule
      <|> parseShow
  eof
  return opts

parseShow :: Parser MkJuvixBlockOptions
parseShow = return MkJuvixBlockOptionsShow

parseHide :: Parser MkJuvixBlockOptions
parseHide = symbol optionHide $> MkJuvixBlockOptionsHide

parseExtractModule :: Parser MkJuvixBlockOptions
parseExtractModule = do
  symbol optionExtractModuleStatements
  dropNum <- fromMaybe 0 <$> optional decimal
  return (MkJuvixBlockOptionsExtractModule (JuvixBlockOptionsExtractModule dropNum))
