module Commands.Dev.Latex.GetJuvixSty.Options where

import CommonOptions

data GetJuvixStyOptions = GetJuvixStyOptions
  deriving stock (Data)

makeLenses ''GetJuvixStyOptions

parseGetJuvixSty :: Parser GetJuvixStyOptions
parseGetJuvixSty = pure GetJuvixStyOptions
