module Juvix.Compiler.Backend.Markdown.Error where

import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error.Pretty
import Juvix.Prelude

data MarkdownBackendError
  = ErrNoMarkdownInfo NoMarkdownInfoError
  | ErrNoJuvixCodeBlocks NoJuvixCodeBlocksError
  deriving stock (Show)

instance ToGenericError MarkdownBackendError where
  genericError = \case
    ErrNoMarkdownInfo e -> genericError e
    ErrNoJuvixCodeBlocks e -> genericError e

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
