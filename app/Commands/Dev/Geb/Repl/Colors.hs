module Commands.Dev.Geb.Repl.Colors where

import Juvix.Prelude
import System.Console.ANSI qualified as Ansi

promptColor :: Ansi.Color
promptColor = Ansi.Blue

nameColor :: Ansi.Color
nameColor = Ansi.Green

typeColor :: Ansi.Color
typeColor = Ansi.Yellow

errorColor :: Ansi.Color
errorColor = Ansi.Red

-- | A string of characters that informs the terminal main format.
mainFormat :: String
mainFormat =
  Ansi.setSGRCode
    [ Ansi.SetConsoleIntensity Ansi.NormalIntensity,
      Ansi.SetColor Ansi.Foreground Ansi.Dull promptColor
    ]

formatIntro :: String
formatIntro =
  Ansi.setSGRCode
    [ Ansi.SetConsoleIntensity Ansi.BoldIntensity,
      Ansi.SetColor Ansi.Foreground Ansi.Dull promptColor
    ]

formatPrompt :: String
formatPrompt =
  Ansi.setSGRCode
    [ Ansi.SetConsoleIntensity Ansi.BoldIntensity,
      Ansi.SetColor Ansi.Foreground Ansi.Vivid promptColor
    ]

formatName :: String
formatName =
  Ansi.setSGRCode
    [ Ansi.SetColor
        Ansi.Foreground
        Ansi.Dull
        nameColor
    ]

formatType :: String
formatType =
  Ansi.setSGRCode
    [ Ansi.SetConsoleIntensity Ansi.NormalIntensity,
      Ansi.SetColor Ansi.Foreground Ansi.Dull typeColor
    ]

formatError :: String
formatError =
  Ansi.setSGRCode
    [ Ansi.SetConsoleIntensity Ansi.NormalIntensity,
      Ansi.SetColor Ansi.Foreground Ansi.Dull errorColor
    ]

-- | Sequence of characters that cleans all the format.
end :: String
end = Ansi.setSGRCode []
