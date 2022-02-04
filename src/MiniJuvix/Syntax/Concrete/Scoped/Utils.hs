module MiniJuvix.Syntax.Concrete.Scoped.Utils where

import MiniJuvix.Syntax.Concrete.Language
import qualified MiniJuvix.Syntax.Concrete.Scoped.Name as S
import MiniJuvix.Utils.Prelude
import qualified Data.HashMap.Strict as HashMap

data ScopedModule = forall t. MkScopedModule (SModuleIsTop t) (Module 'Scoped t)

mkScopedModule :: forall t. SingI t => Module 'Scoped t -> ScopedModule
mkScopedModule = MkScopedModule sing

getAllModules :: Module 'Scoped 'ModuleTop -> HashMap S.NameId (Module 'Scoped 'ModuleTop)
getAllModules m =
   HashMap.fromList $ singl m : [ singl n | Import n <- allImports (mkScopedModule m) ]
  where
  allImports :: ScopedModule -> [Import 'Scoped]
  allImports (MkScopedModule _ w) =
    concat [ i : allImports (mkScopedModule t) | StatementImport i@(Import t) <- moduleBody w ]
    <> concatMap (allImports . mkScopedModule ) [ l | StatementModule l <- moduleBody w]
  singl :: Module 'Scoped 'ModuleTop -> (S.NameId, Module 'Scoped 'ModuleTop)
  singl n = (S._nameId (modulePath n), n)
