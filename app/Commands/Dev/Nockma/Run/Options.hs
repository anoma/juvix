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
  command "builtin-client" $
    info
      (NockmaRunBuiltinClient <$> parseNockmaRunBuiltinClientOptions)
      (progDesc "Run an Anoma program using the builtin Nockma client")

commandRunEphemeralClient :: Mod CommandFields NockmaRunCommand
commandRunEphemeralClient =
  command "ephemeral-client" $
    info
      (NockmaRunEphemeralClient <$> parseNockmaRunEphemeralClientOptions)
      (progDesc "Run an Anoma program with an ephemeral Anoma client")

commandRunWithClient :: Mod CommandFields NockmaRunCommand
commandRunWithClient =
  command "with-client" $
    info
      (NockmaRunWithClient <$> parseNockmaRunWithClientOptions)
      (progDesc "Run an Anoma program with an with Anoma client")
