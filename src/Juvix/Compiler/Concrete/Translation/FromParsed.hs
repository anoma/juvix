module Juvix.Compiler.Concrete.Translation.FromParsed
  ( module Juvix.Compiler.Concrete.Translation.FromParsed,
    module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping,
    module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Data.Context,
  )
where

import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Data.Context
import Juvix.Compiler.Concrete.Translation.FromSource.Data.Context qualified as Parsed
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Store.Language
import Juvix.Prelude

fromParsed ::
  (Members '[Reader EntryPoint, Reader ModuleTable, Reader Parsed.ParserResult, Error JuvixError, NameIdGen] r) =>
  Sem r ScoperResult
fromParsed = do
  e <- ask
  tab <- ask
  r <- ask
  let tab' = fmap (^. moduleInfoScopedModule) (tab ^. moduleTable)
  scopeCheck e tab' r
