module Juvix.Compiler.Backend.Markdown.Error where

import Juvix.Compiler.Backend.Markdown.Data.Types
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error.Pretty
import Juvix.Parser.Error.Base
import Juvix.Prelude

data MarkdownBackendError
  = ErrInternalNoMarkdownInfo NoMarkdownInfoError
  | ErrNoJuvixCodeBlocks NoJuvixCodeBlocksError
  | ErrInvalidExtractModuleBlock InvalidExtractModuleBlockError
  | ErrInvalidCodeBlockAttribtues InvalidCodeBlockAttributesError
  deriving stock (Show)

instance ToGenericError MarkdownBackendError where
  genericError = \case
    ErrInternalNoMarkdownInfo e -> genericError e
    ErrNoJuvixCodeBlocks e -> genericError e
    ErrInvalidExtractModuleBlock e -> genericError e
    ErrInvalidCodeBlockAttribtues e -> genericError e

newtype NoMarkdownInfoError = NoMarkdownInfoError
  { _noMarkdownInfoFilepath :: Path Abs File
  }
  deriving stock (Show)

instance ToGenericError NoMarkdownInfoError where
  genericError NoMarkdownInfoError {..} = do
    let msg = "The markdown file is empty:\n" <+> pretty _noMarkdownInfoFilepath
    return
      GenericError
        { _genericErrorLoc = i,
          _genericErrorMessage = prettyError msg,
          _genericErrorIntervals = [i]
        }
    where
      i :: Interval
      i = singletonInterval . mkInitialLoc $ _noMarkdownInfoFilepath

newtype NoJuvixCodeBlocksError = NoJuvixCodeBlocksError
  { _noJuvixCodeBlocksErrorFilepath :: Path Abs File
  }
  deriving stock (Show)

instance ToGenericError NoJuvixCodeBlocksError where
  genericError NoJuvixCodeBlocksError {..} = do
    let msg = "The markdown file contain no Juvix code blocks:\n" <+> pretty _noJuvixCodeBlocksErrorFilepath
    return
      GenericError
        { _genericErrorLoc = i,
          _genericErrorMessage = prettyError msg,
          _genericErrorIntervals = [i]
        }
    where
      i :: Interval
      i = singletonInterval . mkInitialLoc $ _noJuvixCodeBlocksErrorFilepath

data InvalidExtractModuleBlockError = InvalidExtractModuleBlockError
  { _invalidExtractModuleBlockErrorInterval :: Maybe Interval,
    _invalidExtractModuleBlockErrorPath :: Path Abs File
  }
  deriving stock (Show)

instance ToGenericError InvalidExtractModuleBlockError where
  genericError InvalidExtractModuleBlockError {..} = do
    let msg :: Doc Ann
        msg = "Juvix code blocks with attribute" <+> optionExtractModuleStatements <+> "must contain a single local module"
    return
      GenericError
        { _genericErrorLoc = i,
          _genericErrorMessage = prettyError msg,
          _genericErrorIntervals = [i]
        }
    where
      i :: Interval
      i =
        fromMaybe
          (singletonInterval (mkInitialLoc (_invalidExtractModuleBlockErrorPath)))
          _invalidExtractModuleBlockErrorInterval

newtype InvalidCodeBlockAttributesError = InvalidCodeBlockAttributesError
  { _invalidCodeBlockAttributesErrorMegaparsecError :: MegaparsecError
  }
  deriving stock (Show)

instance ToGenericError InvalidCodeBlockAttributesError where
  genericError InvalidCodeBlockAttributesError {..} =
    genericError _invalidCodeBlockAttributesErrorMegaparsecError
