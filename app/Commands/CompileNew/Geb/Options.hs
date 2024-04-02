module Commands.CompileNew.Geb.Options
  ( module Commands.CompileNew.Geb.Options,
    module Commands.CompileNew.CommonOptions,
  )
where

import Commands.CompileNew.CommonOptions
import CommonOptions

data GebOptions = GebOptions
  { _gebCompileCommonOptions :: CompileCommonOptionsMain,
    _gebOnlyTerm :: Bool
  }
  deriving stock (Data)

makeLenses ''GebOptions

parseGeb :: Parser GebOptions
parseGeb = do
  _gebCompileCommonOptions <- parseCompileCommonOptionsMain
  _gebOnlyTerm <-
    switch
      ( short 'G' -- TODO I would like to deprecate the short flag
          <> long "only-term"
          <> help "Produce term output only"
      )
  pure GebOptions {..}
