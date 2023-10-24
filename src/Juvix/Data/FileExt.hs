module Juvix.Data.FileExt where

import Data.Text qualified as Text
import Juvix.Prelude.Base
import Juvix.Prelude.Path
import Juvix.Prelude.Pretty
import Prelude (show)

-- | File extensions Juvix interacts with.
data FileExt
  = FileExtJuvix
  | FileExtJuvixMarkdown
  | FileExtJuvixGeb
  | FileExtJuvixCore
  | FileExtJuvixAsm
  | FileExtLisp
  | FileC
  | FileExtMarkdown
  | FileExtHtml
  deriving stock (Eq)

juvixFileExt :: (IsString a) => a
juvixFileExt = ".juvix"

juvixMarkdownFileExt :: (IsString a) => a
juvixMarkdownFileExt = ".juvix.md"

juvixGebFileExt :: (IsString a) => a
juvixGebFileExt = ".geb"

juvixCoreFileExt :: (IsString a) => a
juvixCoreFileExt = ".jvc"

juvixAsmFileExt :: (IsString a) => a
juvixAsmFileExt = ".jva"

lispFileExt :: (IsString a) => a
lispFileExt = ".lisp"

htmlFileExt :: (IsString a) => a
htmlFileExt = ".html"

markdownFileExt :: (IsString a) => a
markdownFileExt = ".md"

cFileExt :: (IsString a) => a
cFileExt = ".c"

fileExtToText :: FileExt -> Text
fileExtToText = \case
  FileExtJuvix -> juvixFileExt
  FileExtJuvixMarkdown -> juvixMarkdownFileExt
  FileExtJuvixGeb -> juvixGebFileExt
  FileExtJuvixCore -> juvixCoreFileExt
  FileExtJuvixAsm -> juvixAsmFileExt
  FileExtLisp -> lispFileExt
  FileC -> cFileExt
  FileExtMarkdown -> markdownFileExt
  FileExtHtml -> htmlFileExt

toMetavar :: FileExt -> String
toMetavar = \case
  FileExtJuvix -> "JUVIX_FILE"
  FileExtJuvixMarkdown -> "JUVIX_MARKDOWN_FILE"
  FileExtJuvixGeb -> "JUVIX_GEB_FILE"
  FileExtJuvixCore -> "JUVIX_CORE_FILE"
  FileExtJuvixAsm -> "JUVIX_ASM_FILE"
  FileExtLisp -> "LISP_FILE"
  FileC -> "C_FILE"
  FileExtMarkdown -> "MARKDOWN_FILE"
  FileExtHtml -> "HTML_FILE"

instance Show FileExt where
  show = Text.unpack . fileExtToText

instance Pretty FileExt where
  pretty = pretty . fileExtToText

isJuvixFile :: Path b File -> Bool
isJuvixFile = (== Just juvixFileExt) . fileExtension

isJuvixMarkdownFile :: Path b File -> Bool
isJuvixMarkdownFile p = case splitExtension p of
  Just (f, ext) -> ext == juvixMarkdownFileExt && isJuvixFile f
  _ -> False

isJuvixGebFile :: Path b File -> Bool
isJuvixGebFile = (== Just juvixGebFileExt) . fileExtension

isJuvixCoreFile :: Path b File -> Bool
isJuvixCoreFile = (== Just juvixCoreFileExt) . fileExtension

isJuvixAsmFile :: Path b File -> Bool
isJuvixAsmFile = (== Just juvixAsmFileExt) . fileExtension

isLispFile :: Path b File -> Bool
isLispFile = (== Just lispFileExt) . fileExtension

isMarkdownFile :: Path b File -> Bool
isMarkdownFile = (== Just markdownFileExt) . fileExtension

isCFile :: Path b File -> Bool
isCFile = (== Just cFileExt) . fileExtension

isHtmlFile :: Path b File -> Bool
isHtmlFile = (== Just htmlFileExt) . fileExtension
