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
  = Export CallsOptions
  | GetJuvixStyOptions GetJuvixStyOptions
  deriving stock (Data)

parseLatexCommand :: Parser LatexCommand
parseLatexCommand =
  hsubparser $
    mconcat
      [ commandExport,
        commandGetSty
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
            (CallGraph <$> parseGetJuvixSty)
            (progDesc "Print juvix.sty to stdout")
