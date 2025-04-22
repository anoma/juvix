module Juvix.Data.FileExt where

import Data.Text qualified as Text
import Juvix.Prelude.Base
import Juvix.Prelude.Pretty
import Path (fileExtension, splitExtension)
import Prelude (show)

-- | File extensions Juvix interacts with.
data FileExt
  = FileExtJuvix
  | FileExtJuvixMarkdown
  | FileExtJuvixCore
  | FileExtJuvixAsm
  | FileExtJuvixReg
  | FileExtJuvixTree
  | FileExtCasm
  | FileExtJson
  | FileExtPlonk
  | FileExtHalo
  | FileExtLisp
  | FileExtRust
  | FileExtC
  | FileExtMarkdown
  | FileExtHtml
  | FileExtCss
  | FileExtNockma
  | FileExtRiscZero
  deriving stock (Eq)

$(genSingletons [''FileExt])

splitExtensions :: Path b File -> (Path b File, [String])
splitExtensions =
  swap
    . run
    . runAccumListReverse
    . go
  where
    go :: (Members '[Accum String] r) => Path b File -> Sem r (Path b File)
    go f = case splitExtension f of
      Nothing -> return f
      Just (f', ext) -> do
        accum ext
        go f'

hasExtensions :: (Foldable l) => Path b File -> l String -> Bool
hasExtensions f exts = toList exts == snd (splitExtensions f)

juvixFileExt :: (IsString a) => a
juvixFileExt = ".juvix"

nockmaDebugFileExts :: (IsString a) => NonEmpty a
nockmaDebugFileExts = ".debug" :| [".nockma"]

nockmaTextFileExts :: (IsString a) => NonEmpty a
nockmaTextFileExts = ".text" :| [".nockma"]

nockmaStorageFileExts :: (IsString a) => NonEmpty a
nockmaStorageFileExts = ".modules" :| [".nockma"]

juvixMarkdownFileExt :: (IsString a) => a
juvixMarkdownFileExt = ".juvix.md"

juvixMarkdownFileExts :: (IsString a) => NonEmpty a
juvixMarkdownFileExts = ".juvix" :| [".md"]

juvixCoreFileExt :: (IsString a) => a
juvixCoreFileExt = ".jvc"

juvixAsmFileExt :: (IsString a) => a
juvixAsmFileExt = ".jva"

juvixRegFileExt :: (IsString a) => a
juvixRegFileExt = ".jvr"

juvixTreeFileExt :: (IsString a) => a
juvixTreeFileExt = ".jvt"

casmFileExt :: (IsString a) => a
casmFileExt = ".casm"

jsonFileExt :: (IsString a) => a
jsonFileExt = ".json"

plonkFileExt :: (IsString a) => a
plonkFileExt = ".plonk"

haloFileExt :: (IsString a) => a
haloFileExt = ".halo2"

lispFileExt :: (IsString a) => a
lispFileExt = ".lisp"

htmlFileExt :: (IsString a) => a
htmlFileExt = ".html"

markdownFileExt :: (IsString a) => a
markdownFileExt = ".md"

isabelleFileExt :: (IsString a) => a
isabelleFileExt = ".thy"

cFileExt :: (IsString a) => a
cFileExt = ".c"

rustFileExt :: (IsString a) => a
rustFileExt = ".rs"

cssFileExt :: (IsString a) => a
cssFileExt = ".css"

nockmaFileExt :: (IsString a) => a
nockmaFileExt = ".nockma"

riscZeroFileExt :: (IsString a) => a
riscZeroFileExt = ".risc0"

fileExtToString :: FileExt -> String
fileExtToString = fileExtToIsString

fileExtToIsString :: (IsString a) => FileExt -> a
fileExtToIsString = \case
  FileExtJuvix -> juvixFileExt
  FileExtJuvixMarkdown -> juvixMarkdownFileExt
  FileExtJuvixCore -> juvixCoreFileExt
  FileExtJuvixAsm -> juvixAsmFileExt
  FileExtJuvixReg -> juvixRegFileExt
  FileExtJuvixTree -> juvixTreeFileExt
  FileExtCasm -> casmFileExt
  FileExtJson -> jsonFileExt
  FileExtPlonk -> plonkFileExt
  FileExtHalo -> haloFileExt
  FileExtLisp -> lispFileExt
  FileExtRust -> rustFileExt
  FileExtC -> cFileExt
  FileExtMarkdown -> markdownFileExt
  FileExtHtml -> htmlFileExt
  FileExtCss -> cssFileExt
  FileExtNockma -> nockmaFileExt
  FileExtRiscZero -> riscZeroFileExt

fileExtToText :: FileExt -> Text
fileExtToText = fileExtToIsString

toMetavar :: FileExt -> String
toMetavar = \case
  FileExtJuvix -> "JUVIX_FILE"
  FileExtJuvixMarkdown -> "JUVIX_MARKDOWN_FILE"
  FileExtJuvixCore -> "JUVIX_CORE_FILE"
  FileExtJuvixAsm -> "JUVIX_ASM_FILE"
  FileExtJuvixReg -> "JUVIX_REG_FILE"
  FileExtJuvixTree -> "JUVIX_TREE_FILE"
  FileExtCasm -> "CASM_FILE"
  FileExtJson -> "JSON_FILE"
  FileExtPlonk -> "PLONK_FILE"
  FileExtHalo -> "HALO_FILE"
  FileExtLisp -> "LISP_FILE"
  FileExtRust -> "RUST_FILE"
  FileExtC -> "C_FILE"
  FileExtMarkdown -> "MARKDOWN_FILE"
  FileExtHtml -> "HTML_FILE"
  FileExtCss -> "CSS_FILE"
  FileExtNockma -> "NOCKMA_FILE"
  FileExtRiscZero -> "RISC0_PROJECT"

instance Show FileExt where
  show = Text.unpack . fileExtToText

instance Pretty FileExt where
  pretty = pretty . fileExtToText

isHidden :: Path b File -> Bool
isHidden = (== ".") . take 1 . toFilePath

isJuvixOrJuvixMdFile :: Path b File -> Bool
isJuvixOrJuvixMdFile = isJuvixFile .||. isJuvixMarkdownFile

isJuvixFile :: Path b File -> Bool
isJuvixFile = (not . isHidden) .&&. ((== Just juvixFileExt) . fileExtension)

isJuvixMarkdownFile :: Path b File -> Bool
isJuvixMarkdownFile p = case splitExtension p of
  Just (f, ext) -> ext == markdownFileExt && isJuvixFile f
  _ -> False

isJuvixCoreFile :: Path b File -> Bool
isJuvixCoreFile = (== Just juvixCoreFileExt) . fileExtension

isPlonkFile :: Path b File -> Bool
isPlonkFile = (== Just plonkFileExt) . fileExtension

isHaloFile :: Path b File -> Bool
isHaloFile = (== Just haloFileExt) . fileExtension

isJuvixAsmFile :: Path b File -> Bool
isJuvixAsmFile = (== Just juvixAsmFileExt) . fileExtension

isJuvixRegFile :: Path b File -> Bool
isJuvixRegFile = (== Just juvixRegFileExt) . fileExtension

isJuvixTreeFile :: Path b File -> Bool
isJuvixTreeFile = (== Just juvixTreeFileExt) . fileExtension

isCasmFile :: Path b File -> Bool
isCasmFile = (== Just casmFileExt) . fileExtension

isLispFile :: Path b File -> Bool
isLispFile = (== Just lispFileExt) . fileExtension

isMarkdownFile :: Path b File -> Bool
isMarkdownFile = (== Just markdownFileExt) . fileExtension

isRustFile :: Path b File -> Bool
isRustFile = (== Just rustFileExt) . fileExtension

isCFile :: Path b File -> Bool
isCFile = (== Just cFileExt) . fileExtension

isHtmlFile :: Path b File -> Bool
isHtmlFile = (== Just htmlFileExt) . fileExtension

isCssFile :: Path b File -> Bool
isCssFile = (== Just cssFileExt) . fileExtension

isNockmaFile :: Path b File -> Bool
isNockmaFile = (== Just nockmaFileExt) . fileExtension

toFileExt :: Path b File -> Maybe FileExt
toFileExt p
  | isJuvixFile p = Just FileExtJuvix
  | isJuvixMarkdownFile p = Just FileExtJuvixMarkdown
  | isJuvixCoreFile p = Just FileExtJuvixCore
  | isJuvixAsmFile p = Just FileExtJuvixAsm
  | isJuvixRegFile p = Just FileExtJuvixReg
  | isJuvixTreeFile p = Just FileExtJuvixTree
  | isCasmFile p = Just FileExtCasm
  | isPlonkFile p = Just FileExtPlonk
  | isHaloFile p = Just FileExtHalo
  | isLispFile p = Just FileExtLisp
  | isRustFile p = Just FileExtRust
  | isCFile p = Just FileExtC
  | isMarkdownFile p = Just FileExtMarkdown
  | isHtmlFile p = Just FileExtHtml
  | isCssFile p = Just FileExtCss
  | isNockmaFile p = Just FileExtNockma
  | otherwise = Nothing

fileExtension' :: Path b File -> String
fileExtension' p = case toFileExt p of
  Just ext -> Text.unpack $ fileExtToText ext
  Nothing -> mconcat $ fileExtension p
