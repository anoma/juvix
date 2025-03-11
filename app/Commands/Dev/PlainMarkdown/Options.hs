module Commands.Dev.PlainMarkdown.Options where

import Commands.Dev.PlainMarkdown.Format.Options
import CommonOptions

data PlainMarkdownCommand
  = Format FormatOptions
  deriving stock (Data)

parsePlainMarkdownCommand :: Parser PlainMarkdownCommand
parsePlainMarkdownCommand =
  hsubparser $
    mconcat
      [ commandFormat
      ]
  where
    commandFormat :: Mod CommandFields PlainMarkdownCommand
    commandFormat = command "format" formatInfo
      where
        formatInfo :: ParserInfo PlainMarkdownCommand
        formatInfo =
          info
            (Format <$> parseFormatOptions)
            (progDesc "Format a plain markdown file (no Juvix involved)")
