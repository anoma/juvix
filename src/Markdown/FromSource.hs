-- | Import this module qualified
module Markdown.FromSource where

import Commonmark.Parser
import Juvix.Prelude
import Markdown.Language
import Text.Show.Pretty

fromFile :: (Members '[Files, Error SimpleError] r) => Path Abs File -> Sem r Blocks
fromFile inputFile = do
  txt <- readFile' inputFile
  case commonmark (toFilePath inputFile) txt of
    Left _err -> error "parse error"
    Right block -> return block

testFile :: Path Abs File -> IO ()
testFile f = runM . runFilesIO . runSimpleErrorIO $ do
  b <- fromFile f
  print (ppShow (b ^. blocks))
  putStrLn "================"
  putStrLn "================\n"
  print b
