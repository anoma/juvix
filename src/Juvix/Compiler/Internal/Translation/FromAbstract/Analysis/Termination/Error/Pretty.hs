module Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination.Error.Pretty
  ( module Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination.Error.Pretty,
    module Juvix.Data.CodeAnn,
  )
where

import Juvix.Data.CodeAnn
import Juvix.Prelude
import Juvix.Prelude.Pretty

highlight :: Doc Ann -> Doc Ann
highlight = annotate AnnCode
