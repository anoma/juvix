module Juvix.Data.HtmlTheme where

import Juvix.Prelude.Base

data HtmlTheme
  = Nord
  | Ayu
  deriving stock (Show, Enum, Bounded, Data)

allThemes :: [HtmlTheme]
allThemes = allElements

parseTheme :: String -> Either String HtmlTheme
parseTheme s = case map toLower s of
  "nord" -> Right Nord
  "ayu" -> Right Ayu
  _ -> Left $ "unrecognised theme: " <> s
