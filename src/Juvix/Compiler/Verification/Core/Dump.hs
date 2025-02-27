module Juvix.Compiler.Verification.Core.Dump
  ( Dumper,
    ppModule,
    dump,
    dump',
  )
where

import Juvix.Compiler.Core.Data.Module qualified as Core
import Juvix.Compiler.Core.Transformation.Optimize.FilterUnreachable qualified as Core
import Juvix.Compiler.Verification.Core.Pretty
import Juvix.Compiler.Verification.Core.Translation.FromCore
import Juvix.Compiler.Verification.Dumper (DumpInfo (..), Dumper, Lang (..))
import Juvix.Compiler.Verification.Dumper qualified as Dumper
import Juvix.Prelude

ppModule :: Core.Module -> Text
ppModule = ppPrint . fromCore . (^. Core.moduleInfoTable) . Core.filterUnreachable . Core.combineInfoTables

dump :: (Member Dumper r) => Text -> Core.Module -> Sem r ()
dump phase = Dumper.dump . DumpInfo LangCore phase . ppModule

dump' :: (Member Dumper r) => Text -> Core.Module -> Sem r Core.Module
dump' phase md = dump phase md >> return md
