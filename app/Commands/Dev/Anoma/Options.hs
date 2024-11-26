module Commands.Dev.Anoma.Options where

import Commands.Dev.Anoma.AddTransaction.Options
import Commands.Dev.Anoma.Prove.Options
import Commands.Dev.Anoma.Start.Options
import CommonOptions

data AnomaCommand
  = AnomaCommandStart StartOptions
  | AnomaCommandStatus
  | AnomaCommandStop
  | AnomaCommandProve ProveOptions
  | AnomaCommandAddTransaction AddTransactionOptions
  deriving stock (Data)

parseAnomaCommand :: Parser AnomaCommand
parseAnomaCommand =
  hsubparser
    ( mconcat
        [ commandStart,
          commandStatus,
          commandStop,
          commandProve,
          commandAddTransaction
        ]
    )
  where
    commandStart :: Mod CommandFields AnomaCommand
    commandStart = command "start" runInfo
      where
        runInfo :: ParserInfo AnomaCommand
        runInfo =
          info
            (AnomaCommandStart <$> parseStartOptions)
            (progDesc "Start an Anoma client")

    commandStatus :: Mod CommandFields AnomaCommand
    commandStatus = command "status" runInfo
      where
        runInfo :: ParserInfo AnomaCommand
        runInfo =
          info
            (pure AnomaCommandStatus)
            (progDesc "Show the status of the Anoma client")

    commandStop :: Mod CommandFields AnomaCommand
    commandStop = command "stop" runInfo
      where
        runInfo :: ParserInfo AnomaCommand
        runInfo =
          info
            (pure AnomaCommandStop)
            (progDesc "Stop the Anoma client")

    commandProve :: Mod CommandFields AnomaCommand
    commandProve = command "prove" runInfo
      where
        runInfo :: ParserInfo AnomaCommand
        runInfo =
          info
            (AnomaCommandProve <$> parseProveOptions)
            ( headerDoc
                ( Just
                    ( vsep
                        ( [ "The prove command submits a Nockma program to the Anoma.Protobuf.NockService.Prove gRPC endpoint.",
                            ""
                          ]
                            <> configHelp
                            <> [ "",
                                 "The gRPC response (a Nockma program) is saved to a file named <input>.proved.nockma, where <input> is the base name of the input file.",
                                 "Use the -o/--output option to specify a custom output filename.",
                                 "",
                                 "If the program generates traces, they will be written to standard output."
                               ]
                        )
                    )
                )
                <> progDesc "Submit a Nockma program to Anoma.Protobuf.NockService.Prove"
            )

    commandAddTransaction :: Mod CommandFields AnomaCommand
    commandAddTransaction = command "add-transaction" runInfo
      where
        runInfo :: ParserInfo AnomaCommand
        runInfo =
          info
            (AnomaCommandAddTransaction <$> parseAddTransactionOptions)
            ( headerDoc
                ( Just
                    ( vsep
                        ( [ "The add-transaction command submits a Nockma transaction candidate to the Anoma.Protobuf.Mempool.AddTransaction gRPC endpoint.",
                            ""
                          ]
                            <> configHelp
                        )
                    )
                )
                <> progDesc "Submit a Nockma transaction candidate to Anoma.Protobuf.Mempool.AddTransaction"
            )

configHelp :: [Doc AnsiStyle]
configHelp =
  [ "By default, the gRPC request is made to the client that is started by juvix dev anoma start.",
    "Use the -c/--config option to use a different Anoma client.",
    "The config file format is:",
    "",
    "url: <ANOMA_CLIENT_URL>",
    "port: <ANOMA_CLIENT_GRPC_PORT>",
    "nodeid: <ANOMA_CLIENT_NODE_ID>"
  ]
