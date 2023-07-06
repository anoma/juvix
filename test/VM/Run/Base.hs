module VM.Run.Base where

import Base
import Data.ByteString.Lazy qualified as BS
import Juvix.Compiler.VM.Extra.Labels
import Juvix.Compiler.VM.Interpreter
import Juvix.Compiler.VM.Language
import Juvix.Compiler.VM.Options qualified as VM
import Juvix.Compiler.VM.Translation.FromSource
import Juvix.Data.PPOutput
import Juvix.Prelude.Aeson

runVM ::
  VM.Options ->
  [Instruction] ->
  IO (Either Text Int)
runVM opts instrs = do
  mp <- case opts ^. VM.optInputsFile of
    Just file -> do
      r <- eitherDecode <$> BS.readFile (toFilePath file)
      case r of
        Left err -> return $ Left (fromString err)
        Right m -> return $ Right $ m ^. jsonIntDataMap
    Nothing -> return $ Right mempty
  case mp of
    Left err -> return $ Left err
    Right m -> do
      let r :: Either LabelError [Instruction] = run $ runError $ resolveLabels instrs
      case r of
        Left err ->
          return $ Left $ show (pretty err)
        Right instrs' ->
          return $ Right $ runCode opts m instrs'

vmRunAssertion' :: [Instruction] -> Path Abs File -> (String -> IO ()) -> Assertion
vmRunAssertion' instrs dataFile step = do
  step "Interpret"
  r <- runVM opts instrs
  case r of
    Left err -> assertFailure (fromText err)
    Right v -> do
      step "Check program output"
      assertEqual "Check: RUN output = 1" v 1
  where
    opts = VM.defaultOptions {VM._optInputsFile = Just dataFile}

vmRunAssertion :: Path Abs File -> Path Abs File -> (String -> IO ()) -> Assertion
vmRunAssertion mainFile dataFile step = do
  step "Parse"
  r <- parseFile mainFile
  case r of
    Left err -> assertFailure (show (pretty err))
    Right instrs -> do
      vmRunAssertion' instrs dataFile step

parseFile :: Path Abs File -> IO (Either MegaparsecError [Instruction])
parseFile f = do
  let f' = toFilePath f
  s <- readFile f'
  return $ runParser f' s
