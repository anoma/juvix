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
  | FileExtCasm
  | FileExtVampIR
  | FileExtVampIRParams
  | FileExtPlonk
  | FileExtHalo
  | FileExtLisp
  | FileExtC
  | FileExtMarkdown
  | FileExtHtml
  | FileExtCss
  | FileExtNockma
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

casmFileExt :: (IsString a) => a
casmFileExt = ".casm"

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

cssFileExt :: (IsString a) => a
cssFileExt = ".css"

nockmaFileExt :: (IsString a) => a
nockmaFileExt = ".nockma"

fileExtToText :: FileExt -> Text
fileExtToText = \case
  FileExtJuvix -> juvixFileExt
  FileExtJuvixMarkdown -> juvixMarkdownFileExt
  FileExtJuvixGeb -> juvixGebFileExt
  FileExtJuvixCore -> juvixCoreFileExt
  FileExtJuvixAsm -> juvixAsmFileExt
  FileExtCasm -> casmFileExt
  FileExtVampIR -> vampIRFileExt
  FileExtVampIRParams -> vampIRParamsFileExt
  FileExtPlonk -> plonkFileExt
  FileExtHalo -> haloFileExt
  FileExtLisp -> lispFileExt
  FileExtC -> cFileExt
  FileExtMarkdown -> markdownFileExt
  FileExtHtml -> htmlFileExt
  FileExtCss -> cssFileExt
  FileExtNockma -> nockmaFileExt

toMetavar :: FileExt -> String
toMetavar = \case
  FileExtJuvix -> "JUVIX_FILE"
  FileExtJuvixMarkdown -> "JUVIX_MARKDOWN_FILE"
  FileExtJuvixGeb -> "JUVIX_GEB_FILE"
  FileExtJuvixCore -> "JUVIX_CORE_FILE"
  FileExtJuvixAsm -> "JUVIX_ASM_FILE"
  FileExtCasm -> "CASM_FILE"
  FileExtVampIR -> "VAMPIR_FILE"
  FileExtVampIRParams -> "VAMPIR_PARAMS_FILE"
  FileExtPlonk -> "PLONK_FILE"
  FileExtHalo -> "HALO_FILE"
  FileExtLisp -> "LISP_FILE"
  FileExtC -> "C_FILE"
  FileExtMarkdown -> "MARKDOWN_FILE"
  FileExtHtml -> "HTML_FILE"
  FileExtCss -> "CSS_FILE"
  FileExtNockma -> "NOCKMA_FILE"

instance Show FileExt where
  show = Text.unpack . fileExtToText

instance Pretty FileExt where
  pretty = pretty . fileExtToText

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

isCasmFile :: Path b File -> Bool
isCasmFile = (== Just casmFileExt) . fileExtension

isLispFile :: Path b File -> Bool
isLispFile = (== Just lispFileExt) . fileExtension

isMarkdownFile :: Path b File -> Bool
isMarkdownFile = (== Just markdownFileExt) . fileExtension

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
  | isCasmFile p = Just FileExtCasm
  | isVampIRFile p = Just FileExtVampIR
  | isVampIRParamsFile p = Just FileExtVampIRParams
  | isPlonkFile p = Just FileExtPlonk
  | isHaloFile p = Just FileExtHalo
  | isLispFile p = Just FileExtLisp
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
