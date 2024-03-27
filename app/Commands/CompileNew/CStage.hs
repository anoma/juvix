-- | TODO where to name and put this file?
module Commands.CompileNew.CStage where

import CommonOptions
import Juvix.Prelude.Pretty
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
            ( "DEPRECATED: Produce C output only. Use `--"
                <> cstageStr
                <> " "
                <> Prelude.show CSource
                <> "source` instead"
            )
      )
  onlyPreprocess <-
    switch
      ( short 'E'
          <> long "only-preprocess"
          <> help
            ( "DEPRECATED: Run the C preprocessor only. Use `--"
                <> cstageStr
                <> " "
                <> Prelude.show CPreprocess
                <> "` instead"
            )
      )
  onlyAssemble <-
    switch
      ( short 'S'
          <> long "only-assemble"
          <> help
            ( "DEPRECATED: Produce assembly output only. Use Use `--"
                <> cstageStr
                <> " "
                <> Prelude.show CAssembly
                <> "` instead"
            )
      )

  mcstage :: Maybe CStage <-
    option
      (Just <$> enumReader (Proxy @CStage))
      ( long cstageStr
          <> metavar "CSTAGE"
          <> value Nothing
          <> completer cstageCompleter
          <> help "Stop the compilation after the specified stage. Hint: use autocomplete"
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
