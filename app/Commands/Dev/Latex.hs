module Commands.Dev.Latex where

import Commands.Base
import Commands.Dev.Latex.Export qualified as Export
import Commands.Dev.Latex.GetJuvixSty qualified as GetJuvixSty
import Commands.Dev.Latex.Options

runCommand :: (Members AppEffects r) => LatexCommand -> Sem r ()
runCommand = \case
  Export m -> Export.runCommand m
  GetJuvixSty m -> GetJuvixSty.runCommand m
