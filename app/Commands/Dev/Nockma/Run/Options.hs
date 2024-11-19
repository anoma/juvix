module Commands.Dev.Nockma.Run.Options
  ( module Commands.Dev.Nockma.Run.BuiltinClient.Options,
    module Commands.Dev.Nockma.Run.EphemeralClient.Options,
    module Commands.Dev.Nockma.Run.WithClient.Options,
    module Commands.Dev.Nockma.Run.Options,
  )
where

import Commands.Dev.Nockma.Run.BuiltinClient.Options
import Commands.Dev.Nockma.Run.EphemeralClient.Options
import Commands.Dev.Nockma.Run.WithClient.Options
import CommonOptions

data NockmaRunCommand
  = NockmaRunBuiltinClient NockmaRunBuiltinClientOptions
  | NockmaRunEphemeralClient NockmaRunEphemeralClientOptions
  | NockmaRunWithClient NockmaRunWithClientOptions
  deriving stock (Data)

makeLenses ''NockmaRunCommand

parseNockmaRunCommand :: Parser NockmaRunCommand
parseNockmaRunCommand =
  hsubparser
    ( mconcat
        [ commandRunBuiltinClient,
          commandRunEphemeralClient,
          commandRunWithClient
        ]
    )

commandRunBuiltinClient :: Mod CommandFields NockmaRunCommand
commandRunBuiltinClient =
  command "builtin-evaluator" $
    info
      (NockmaRunBuiltinClient <$> parseNockmaRunBuiltinClientOptions)
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
