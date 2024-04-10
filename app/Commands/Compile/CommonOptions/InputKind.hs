{-# LANGUAGE AllowAmbiguousTypes #-}

module Commands.Compile.CommonOptions.InputKind where

import App
import CommonOptions

data InputKind
  = -- | The input is a .juvix or .juvix.md file. If omitted, the main in juvix.yaml is used
    InputMain
  | -- | The input is a non-optional file with some extension
    InputExtension FileExt

$(genSingletons [''InputKind])

type InputFileType :: InputKind -> GHCType
type family InputFileType s = res where
  InputFileType 'InputMain = Maybe (AppPath File)
  InputFileType ('InputExtension _) = AppPath File

parseInputFileType :: forall k. (SingI k) => Parser (InputFileType k)
parseInputFileType = case sing :: SInputKind k of
  SInputMain -> optional (parseInputFiles (FileExtJuvix :| [FileExtMarkdown]))
  SInputExtension inputExtension -> parseInputFile (fromSing inputExtension)
