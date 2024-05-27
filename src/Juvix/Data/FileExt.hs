module Juvix.Data.FileExt where

import Data.Text qualified as Text
import Juvix.Prelude.Base
import Juvix.Prelude.Pretty
import Path (File, Path, fileExtension, splitExtension)
import Prelude (show)

-- | File extensions Juvix interacts with.
data FileExt
  = FileExtJuvix
  | FileExtJuvixMarkdown
  | FileExtJuvixGeb
  | FileExtJuvixCore
  | FileExtJuvixAsm
  | FileExtJuvixReg
  | FileExtJuvixTree
  | FileExtCasm
  | FileExtJson
  | FileExtVampIR
  | FileExtVampIRParams
  | FileExtPlonk
  | FileExtHalo
  | FileExtLisp
  | FileExtRust
  | FileExtC
  | FileExtMarkdown
  | FileExtHtml
  | FileExtCss
  | FileExtNockma
  deriving stock (Eq)

$(genSingletons [''FileExt])

juvixFileExt :: (IsString a) => a
juvixFileExt = ".juvix"

juvixMarkdownFileExt :: (IsString a) => a
juvixMarkdownFileExt = ".juvix.md"

juvixMarkdownFileExts :: (IsString a) => NonEmpty a
juvixMarkdownFileExts = ".juvix" :| [".md"]

juvixGebFileExt :: (IsString a) => a
juvixGebFileExt = ".geb"

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

vampIRFileExt :: (IsString a) => a
vampIRFileExt = ".pir"

vampIRParamsFileExt :: (IsString a) => a
vampIRParamsFileExt = ".pp"

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

cFileExt :: (IsString a) => a
cFileExt = ".c"

rustFileExt :: (IsString a) => a
rustFileExt = ".rs"

cssFileExt :: (IsString a) => a
cssFileExt = ".css"

nockmaFileExt :: (IsString a) => a
nockmaFileExt = ".nockma"

fileExtToString :: FileExt -> String
fileExtToString = fileExtToIsString

fileExtToIsString :: (IsString a) => FileExt -> a
fileExtToIsString = \case
  FileExtJuvix -> juvixFileExt
  FileExtJuvixMarkdown -> juvixMarkdownFileExt
  FileExtJuvixGeb -> juvixGebFileExt
  FileExtJuvixCore -> juvixCoreFileExt
  FileExtJuvixAsm -> juvixAsmFileExt
  FileExtJuvixReg -> juvixRegFileExt
  FileExtJuvixTree -> juvixTreeFileExt
  FileExtCasm -> casmFileExt
  FileExtJson -> jsonFileExt
  FileExtVampIR -> vampIRFileExt
  FileExtVampIRParams -> vampIRParamsFileExt
  FileExtPlonk -> plonkFileExt
  FileExtHalo -> haloFileExt
  FileExtLisp -> lispFileExt
  FileExtRust -> rustFileExt
  FileExtC -> cFileExt
  FileExtMarkdown -> markdownFileExt
  FileExtHtml -> htmlFileExt
  FileExtCss -> cssFileExt
  FileExtNockma -> nockmaFileExt

fileExtToText :: FileExt -> Text
fileExtToText = fileExtToIsString

toMetavar :: FileExt -> String
toMetavar = \case
  FileExtJuvix -> "JUVIX_FILE"
  FileExtJuvixMarkdown -> "JUVIX_MARKDOWN_FILE"
  FileExtJuvixGeb -> "JUVIX_GEB_FILE"
  FileExtJuvixCore -> "JUVIX_CORE_FILE"
  FileExtJuvixAsm -> "JUVIX_ASM_FILE"
  FileExtJuvixReg -> "JUVIX_REG_FILE"
  FileExtJuvixTree -> "JUVIX_TREE_FILE"
  FileExtCasm -> "CASM_FILE"
  FileExtJson -> "JSON_FILE"
  FileExtVampIR -> "VAMPIR_FILE"
  FileExtVampIRParams -> "VAMPIR_PARAMS_FILE"
  FileExtPlonk -> "PLONK_FILE"
  FileExtHalo -> "HALO_FILE"
  FileExtLisp -> "LISP_FILE"
  FileExtRust -> "RUST_FILE"
  FileExtC -> "C_FILE"
  FileExtMarkdown -> "MARKDOWN_FILE"
  FileExtHtml -> "HTML_FILE"
  FileExtCss -> "CSS_FILE"
  FileExtNockma -> "NOCKMA_FILE"

instance Show FileExt where
  show = Text.unpack . fileExtToText

instance Pretty FileExt where
  pretty = pretty . fileExtToText

isJuvixOrJuvixMdFile :: Path b File -> Bool
isJuvixOrJuvixMdFile = isJuvixFile .||. isJuvixMarkdownFile

isJuvixFile :: Path b File -> Bool
isJuvixFile = (== Just juvixFileExt) . fileExtension

isJuvixMarkdownFile :: Path b File -> Bool
isJuvixMarkdownFile p = case splitExtension p of
  Just (f, ext) -> ext == markdownFileExt && isJuvixFile f
  _ -> False

isJuvixGebFile :: Path b File -> Bool
isJuvixGebFile = (== Just juvixGebFileExt) . fileExtension

isJuvixCoreFile :: Path b File -> Bool
isJuvixCoreFile = (== Just juvixCoreFileExt) . fileExtension

isVampIRFile :: Path b File -> Bool
isVampIRFile = (== Just vampIRFileExt) . fileExtension

isVampIRParamsFile :: Path b File -> Bool
isVampIRParamsFile = (== Just vampIRParamsFileExt) . fileExtension

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
  | isJuvixGebFile p = Just FileExtJuvixGeb
  | isJuvixCoreFile p = Just FileExtJuvixCore
  | isJuvixAsmFile p = Just FileExtJuvixAsm
  | isJuvixRegFile p = Just FileExtJuvixReg
  | isJuvixTreeFile p = Just FileExtJuvixTree
  | isCasmFile p = Just FileExtCasm
  | isVampIRFile p = Just FileExtVampIR
  | isVampIRParamsFile p = Just FileExtVampIRParams
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
