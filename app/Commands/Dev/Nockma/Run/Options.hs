module Commands.Dev.Nockma.Run.Options
  ( module Commands.Dev.Nockma.Run.BuiltinEvaluator.Options,
    module Commands.Dev.Nockma.Run.EphemeralClient.Options,
    module Commands.Dev.Nockma.Run.WithClient.Options,
    module Commands.Dev.Nockma.Run.Options,
  )
where

import Commands.Dev.Nockma.Run.BuiltinEvaluator.Options
import Commands.Dev.Nockma.Run.EphemeralClient.Options
import Commands.Dev.Nockma.Run.WithClient.Options
import CommonOptions

data NockmaRunCommand
  = NockmaRunBuiltinEvaluator NockmaRunBuiltinEvaluatorOptions
  | NockmaRunEphemeralClient NockmaRunEphemeralClientOptions
  | NockmaRunWithClient NockmaRunWithClientOptions
  deriving stock (Data)

makeLenses ''NockmaRunCommand

parseNockmaRunCommand :: Parser NockmaRunCommand
parseNockmaRunCommand =
  hsubparser
    ( mconcat
        [ commandRunBuiltinEvaluator,
          commandRunEphemeralClient,
          commandRunWithClient
        ]
    )

commandRunBuiltinEvaluator :: Mod CommandFields NockmaRunCommand
commandRunBuiltinEvaluator =
  command "builtin-evaluator" $
    info
      (NockmaRunBuiltinEvaluator <$> parseNockmaRunBuiltinEvaluatorOptions)
      (progDesc "Run with the builtin Nockma evaluator")

commandRunEphemeralClient :: Mod CommandFields NockmaRunCommand
commandRunEphemeralClient =
  command "ephemeral-client" $
    info
      (NockmaRunEphemeralClient <$> parseNockmaRunEphemeralClientOptions)
      (progDesc "Run with an ephemeral Anoma client")

commandRunWithClient :: Mod CommandFields NockmaRunCommand
commandRunWithClient =
  command "with-client" $
    info
      (NockmaRunWithClient <$> parseNockmaRunWithClientOptions)
      (progDesc "Run with a running Anoma client")
