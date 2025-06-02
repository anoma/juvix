module Commands.Dev.Latex.Options
  ( module Commands.Dev.Latex.Options,
    module Commands.Dev.Latex.Export.Options,
    module Commands.Dev.Latex.GetJuvixSty.Options,
  )
where

import Commands.Dev.Latex.Export.Options
import Commands.Dev.Latex.GetJuvixSty.Options
import CommonOptions

data LatexCommand
  = Export ExportOptions
  | GetJuvixSty GetJuvixStyOptions
  deriving stock (Data)

parseLatex :: Parser LatexCommand
parseLatex =
  hsubparser
    $ mconcat
      [ commandExport,
        commandGetJuvixSty
      ]
  where
    commandExport :: Mod CommandFields LatexCommand
    commandExport = command "export" minfo
      where
        minfo :: ParserInfo LatexCommand
        minfo =
          info
            (Export <$> parseExport)
            (progDesc "Export a Juvix module to LaTeX")

    commandGetJuvixSty :: Mod CommandFields LatexCommand
    commandGetJuvixSty = command "getJuvixSty" minfo
      where
        minfo :: ParserInfo LatexCommand
        minfo =
          info
            (GetJuvixSty <$> parseGetJuvixSty)
            (progDesc "Print juvix.sty to stdout")
