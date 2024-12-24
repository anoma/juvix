module Commands.Dev.Anoma.Options where

import Commands.Dev.Anoma.AddTransaction.Options
import Commands.Dev.Anoma.Indexer.Options
import Commands.Dev.Anoma.PrintConfig.Options
import Commands.Dev.Anoma.Prove.Options
import Commands.Dev.Anoma.Start.Options
import CommonOptions

data AnomaCommand
  = AnomaCommandStart StartOptions
  | AnomaCommandStatus
  | AnomaCommandPrintConfig PrintConfigOptions
  | AnomaCommandStop
  | AnomaCommandProve ProveOptions
  | AnomaCommandAddTransaction AddTransactionOptions
  | AnomaCommandIndexer AnomaIndexerCommand
  deriving stock (Data)

data AnomaCommandGlobal = AnomaCommandGlobal
  { _anomaCommandGlobalClientConfig :: Maybe (AppPath File),
    _anomaCommandGlobalCommand :: AnomaCommand
  }
  deriving stock (Data)

makeLenses ''AnomaCommandGlobal

parseAnomaCommand :: Parser AnomaCommandGlobal
parseAnomaCommand =
  AnomaCommandGlobal
    <$> optional anomaClientConfigOpt
    <*> hsubparser
      ( mconcat
          [ commandStart,
            commandStatus,
            commandStop,
            commandProve,
            commandPrintConfig,
            commandAddTransaction,
            commandIndexer
          ]
      )
  where
    commandPrintConfig :: Mod CommandFields AnomaCommand
    commandPrintConfig = command "print-config" runInfo
      where
        runInfo :: ParserInfo AnomaCommand
        runInfo =
          info
            (AnomaCommandPrintConfig <$> parsePrintConfigOptions)
            (progDesc "Prints the yaml configuration of the Anoma client to stdout")

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
                            "",
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
            (progDesc "Submit a Nockma transaction candidate to Anoma.Protobuf.Mempool.AddTransaction")

    commandIndexer :: Mod CommandFields AnomaCommand
    commandIndexer =
      command "indexer" $
        info
          (AnomaCommandIndexer <$> parseAnomaIndexerCommand)
          (progDesc "Subcommands related to the Anoma indexer")
