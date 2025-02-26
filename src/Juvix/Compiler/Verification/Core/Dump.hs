module Juvix.Compiler.Verification.Core.Dump where

import Juvix.Compiler.Core qualified as Core
import Juvix.Compiler.Core.Transformation.Optimize.FilterUnreachable qualified as Core
import Juvix.Compiler.Verification.Core.Pretty
import Juvix.Compiler.Verification.Core.Translation.FromCore
import Juvix.Compiler.Verification.Dumper as Dumper
import Juvix.Prelude

ppModule :: Core.Module -> Text
ppModule = ppTrace . fromCore . (^. Core.moduleInfoTable) . Core.filterUnreachable . Core.combineInfoTables

dump :: (Member Dumper r) => Core.Module -> Sem r ()
dump = Dumper.dump LangCore . ppModule
