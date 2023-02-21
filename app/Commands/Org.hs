module Commands.Org where

import Commands.Base
import Commands.Org.Options
import Data.Markdown
import Text.Show.Pretty (ppShow, pPrint)
import Commonmark
import Commonmark.Extensions

runCommand :: Members '[Embed IO, App] r => OrgOptions -> Sem r ()
runCommand opts = do
  let syntaxSpec = attributesSpec <> defaultSyntaxSpec
  f <- someBaseToAbs' (opts ^. orgInputFile . pathPath)
  txt <- liftIO $ readFile (toFilePath f)
  res <- commonmarkWith syntaxSpec (toFilePath f) txt
  case res of
    Left _ -> error "error"
    Right (r :: Blocks) -> liftIO (pPrint r)


-- xx = Blocks {_blocksBlocks = [BlockHeading (Heading
--                                             {_headingLevel = 1,
--                                              _headingInline = Inlines
--                                               {_inlinesInlines = [
--                                                   InlineStr (Str
--                                                              {_strAttr = [],
--                                                               _strSourceRange = mempty,
--                                                               _strText = "Document"}),
--                                                     InlineStr (Str {_strAttr = [],
--                                                                     _strSourceRange = mempty ,
--                                                                     _strText = " "}),
--                                                     InlineStr (Str {_strAttr = [],
--                                                                     _strSourceRange = mempty ,
--                                                                     _strText = "Title"})], _inlinesAttr = [],
--                                                 _inlinesSourceRange = "/home/jan/projects/juvix/tmp.md@1:3-1:17"}})],
--               _blocksAttr = [("class","juvix")],
--               _blocksSourceRange = "/home/jan/projects/juvix/tmp.md@1:1-2:1"}
