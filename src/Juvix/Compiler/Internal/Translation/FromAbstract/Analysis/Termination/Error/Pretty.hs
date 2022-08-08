module Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination.Error.Pretty
  ( module Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination.Error.Pretty,
    module Juvix.Data.CodeAnn,
  )
where

import Juvix.Data.CodeAnn
import Juvix.Data.PPOutput
import Juvix.Prelude
import Juvix.Prelude.Pretty

prettyError :: Doc Ann -> AnsiText
prettyError = AnsiText . PPOutput

indent' :: Doc ann -> Doc ann
indent' = indent 2

prettyT :: Text -> Doc Ann
prettyT = pretty

highlight :: Doc Ann -> Doc Ann
highlight = annotate AnnCode
