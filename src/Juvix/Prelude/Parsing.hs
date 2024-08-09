module Juvix.Prelude.Parsing
  ( module Text.Megaparsec,
    module Juvix.Prelude.Parsing,
    module Text.Megaparsec.Char,
    module Control.Monad.Combinators.Expr,
    module Control.Monad.Combinators.NonEmpty,
    module Control.Monad.Trans.Class,
  )
where

import Control.Monad.Combinators.Expr
import Control.Monad.Combinators.NonEmpty (sepBy1, sepEndBy1, some)
import Control.Monad.Trans.Class (lift)
import Juvix.Prelude.Base
import Juvix.Prelude.Pretty
import Text.Megaparsec hiding (sepBy1, sepEndBy1, some)
import Text.Megaparsec.Char

type ParsecS' txt r = ParsecT Void txt (Sem r)

type ParsecS r = ParsecS' Text r

parseHelper ::
  forall err txt a.
  (TraversableStream txt, VisualStream txt, IsString err) =>
  Parsec Void txt a ->
  txt ->
  Either err a
parseHelper p t = mapLeft ppParseErrorBundle (runParser p "<input>" t)

parseHelperS ::
  forall err txt a r.
  (TraversableStream txt, VisualStream txt, IsString err) =>
  ParsecS' txt r a ->
  txt ->
  Sem r (Either err a)
parseHelperS p t = mapLeft ppParseErrorBundle <$> runParserT p "<input>" t

ppParseErrorBundle :: (TraversableStream txt, VisualStream txt, IsString err) => ParseErrorBundle txt Void -> err
ppParseErrorBundle = prettyIsString . errorBundlePretty
