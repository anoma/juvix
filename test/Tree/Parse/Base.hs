module Tree.Parse.Base where

import Base
import Juvix.Compiler.Tree.Data.InfoTable
import Juvix.Compiler.Tree.Pretty
import Juvix.Compiler.Tree.Translation.FromSource
import Juvix.Data.PPOutput

treeParseAssertion :: Path Abs File -> (String -> IO ()) -> Assertion
treeParseAssertion mainFile step = do
  step "Parse"
  r <- parseFile mainFile
  case r of
    Left err -> assertFailure (prettyString err)
    Right tab -> do
      withTempDir'
        ( \dirPath -> do
            let outputFile = dirPath <//> $(mkRelFile "out.out")
            step "Print"
            writeFileEnsureLn outputFile (ppPrint tab tab)
            step "Parse printed"
            r' <- parseFile outputFile
            case r' of
              Left err -> assertFailure (prettyString err)
              Right tab' -> do
                assertBool ("Check: print . parse = print . parse . print . parse") (ppPrint tab tab == ppPrint tab' tab')
        )

parseFile :: Path Abs File -> IO (Either MegaparsecError InfoTable)
parseFile f = do
  s <- readFile f
  return (runParser f s)
