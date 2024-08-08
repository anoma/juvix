module Juvix.Compiler.Concrete.Translation.FromParsed
  ( module Juvix.Compiler.Concrete.Translation.FromParsed,
    module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping,
    module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Data.Context,
  )
where

import Juvix.Compiler.Builtins.Effect
import Juvix.Compiler.Concrete.Data.Highlight
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Data.Context
import Juvix.Compiler.Concrete.Translation.FromSource.Data.Context qualified as Parsed
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Store.Extra
import Juvix.Compiler.Store.Language
import Juvix.Prelude

fromParsed ::
  ( Members
      '[ HighlightBuilder,
         Reader Package,
         Reader ModuleTable,
         Reader Parsed.ParserResult,
         Error JuvixError,
         NameIdGen,
         Builtins
       ]
      r
  ) =>
  Sem r ScoperResult
fromParsed = do
  e <- ask
  tab <- ask
  r <- ask
  scopeCheck e (getScopedModuleTable tab) r
