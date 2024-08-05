-- | TODO where to name and put this file?
module Commands.Compile.CStage where

import CommonOptions
import Juvix.Prelude as Juvix
import Prelude (show)

data CStage
  = CSource
  | CPreprocess
  | CAssembly
  | CExecutable
  deriving stock (Eq, Ord, Bounded, Enum, Data)

instance Show CStage where
  show = \case
    CSource -> "source"
    CPreprocess -> "preprocess"
    CAssembly -> "assembly"
    CExecutable -> "exec"

instance Pretty CStage where
  pretty = pretty . Prelude.show

cstageHelp :: forall str. (IsString str) => CStage -> str
cstageHelp = \case
  CSource -> "Produce .c code only"
  CPreprocess -> "Run the C preprocessor only"
  CAssembly -> "Produce .s assembly code only"
  CExecutable -> "Produce an executable"

cstageCompleter :: Completer
cstageCompleter = enumCompleter (Proxy @CStage)

cstageStr :: forall str. (IsString str) => str
cstageStr = "cstage"

parseCStage :: Parser CStage
parseCStage = do
  onlySource <-
    switch
      ( short 'C'
          <> long "only-c"
          <> help
            ( "DEPRECATED: Produce C output only. Use '--"
                <> cstageStr
                <> " "
                <> Prelude.show CSource
                <> "source' instead"
            )
      )
  onlyPreprocess <-
    switch
      ( short 'E'
          <> long "only-preprocess"
          <> help
            ( "DEPRECATED: Run the C preprocessor only. Use '--"
                <> cstageStr
                <> " "
                <> Prelude.show CPreprocess
                <> "' instead"
            )
      )
  onlyAssemble <-
    switch
      ( short 'S'
          <> long "only-assemble"
          <> help
            ( "DEPRECATED: Produce assembly output only. Use Use '--"
                <> cstageStr
                <> " "
                <> Prelude.show CAssembly
                <> "' instead"
            )
      )

  mcstage :: Maybe CStage <-
    option
      (Just <$> enumReader (Proxy @CStage))
      ( long cstageStr
          <> metavar "CSTAGE"
          <> value Nothing
          <> completer cstageCompleter
          <> help
            ( "Select the type of output. Available options:\n"
                <> toPlainString (itemize ["'" <> Juvix.show s <> "'" <> ": " <> cstageHelp s | s <- allElements @CStage])
            )
      )
  pure $
    if
        | onlySource -> CSource
        | onlyPreprocess -> CPreprocess
        | onlyAssemble -> CAssembly
        | otherwise -> case mcstage of
            Nothing -> defaultCStage
            Just x -> x

defaultCStage :: CStage
defaultCStage = CExecutable
