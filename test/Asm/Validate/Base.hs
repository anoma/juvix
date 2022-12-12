module Asm.Validate.Base where

import Base
import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Asm.Transformation.Validate
import Juvix.Compiler.Asm.Translation.FromSource

asmValidateErrorAssertion :: Path Abs File -> (String -> IO ()) -> Assertion
asmValidateErrorAssertion mainFile step = do
  step "Parse"
  r <- parseFile (mainFile)
  case r of
    Left _ -> assertBool "" True
    Right tab -> do
      step "Validate"
      case validate' tab of
        Just _ -> assertBool "" True
        Nothing -> assertFailure "no error"

parseFile :: Path Abs File -> IO (Either ParserError InfoTable)
parseFile f = do
  s <- readFile (toFilePath f)
  return $ runParser (toFilePath f) s
