module Commands.Dev.Nockma.Ide.Options where

import Commands.Dev.Nockma.Ide.Highlight.Options
import CommonOptions

data NockmaIdeCommand
  = NockmaIdeHighlight NockmaHighlightOptions
  deriving stock (Data)

parseNockmaIdeCommand :: Parser NockmaIdeCommand
parseNockmaIdeCommand =
  hsubparser $
    mconcat
      [ commandHighlight
      ]
  where
    commandHighlight :: Mod CommandFields NockmaIdeCommand
    commandHighlight = command "highlight" runInfo
      where
        runInfo :: ParserInfo NockmaIdeCommand
        runInfo =
          info
            (NockmaIdeHighlight <$> parseNockmaHighlightOptions)
            (progDesc "Highlight a nockma term (only for Emacs)")
