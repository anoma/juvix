module Commands.Html where

import MiniJuvix.Prelude hiding (Doc)
import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Html
import Options.Applicative

data HtmlOptions = HtmlOptions
  { _htmlRecursive :: Bool,
    _htmlTheme :: Theme
  }

makeLenses ''HtmlOptions

parseHtml :: Parser HtmlOptions
parseHtml = do
  _htmlRecursive <-
    switch
      ( long "recursive"
          <> help "export imported modules recursively"
      )
  _htmlTheme <-
    option
      (eitherReader parseTheme)
      ( long "theme"
          <> metavar "THEME"
          <> value Ayu
          <> showDefault
          <> help "selects a theme: ayu (light); nord (dark)"
      )
  pure HtmlOptions {..}
  where
    parseTheme :: String -> Either String Theme
    parseTheme s = case s of
      "nord" -> Right Nord
      "ayu" -> Right Ayu
      _ -> Left $ "unrecognised theme: " <> s
