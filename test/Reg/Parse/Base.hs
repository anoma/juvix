module Reg.Parse.Base where

import Base
import Juvix.Compiler.Reg.Data.InfoTable
import Juvix.Compiler.Reg.Pretty
import Juvix.Compiler.Reg.Translation.FromSource
import Juvix.Data.PPOutput

regParseAssertion :: Path Abs File -> (String -> IO ()) -> Assertion
regParseAssertion mainFile step = do
  step "Parse"
  r <- parseFile mainFile
  case r of
    Left err -> assertFailure (show (pretty err))
    Right tab -> do
      withTempDir'
        ( \dirPath -> do
            let outputFile = dirPath <//> $(mkRelFile "out.out")
            step "Print"
            writeFileEnsureLn outputFile (ppPrint tab tab)
            step "Parse printed"
            r' <- parseFile outputFile
            case r' of
              Left err -> assertFailure (show (pretty err))
              Right tab' -> do
                assertBool ("Check: print . parse = print . parse . print . parse") (ppPrint tab tab == ppPrint tab' tab')
        )

parseFile :: Path Abs File -> IO (Either MegaparsecError InfoTable)
parseFile f = do
  s <- readFile f
  return (runParser f s)
