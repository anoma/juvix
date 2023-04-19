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

parseHelper :: (TraversableStream txt, VisualStream txt) => Parsec Void txt a -> txt -> Either Text a
parseHelper p t = case runParser p "<input>" t of
  Left (err :: ParseErrorBundle txt Void) -> Left (prettyText (errorBundlePretty err))
  Right r -> return r
