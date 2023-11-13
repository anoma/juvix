module Juvix.Compiler.Backend.Markdown.Error where

import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error.Pretty
import Juvix.Prelude


data MarkdownBackendError 
  = ErrEmptyMarkdown EmptyMarkdownError
  | ErrNoJuvixCodeBlocks NoJuvixCodeBlocksError
  deriving stock (Show)

instance ToGenericError MarkdownBackendError where
  genericError = \case
    ErrEmptyMarkdown e -> genericError e
    ErrNoJuvixCodeBlocks e -> genericError e

newtype EmptyMarkdownError = EmptyMarkdownError
  { _emptyMarkdownErrorFilepath :: Path Abs File 
  }
  deriving stock (Show)

instance ToGenericError EmptyMarkdownError where
  genericError EmptyMarkdownError {..} = do
    let msg = "The markdown file is empty:\n" <+> pretty _emptyMarkdownErrorFilepath
    return
      GenericError
        { _genericErrorLoc = i,
          _genericErrorMessage = prettyError msg,
          _genericErrorIntervals = [i]
        }
    where
      i :: Interval
      i = singletonInterval . mkInitialLoc $ _emptyMarkdownErrorFilepath


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