module MiniJuvix.Syntax.Concrete.Scoped.Utils where

import Data.HashMap.Strict qualified as HashMap
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Language
import MiniJuvix.Syntax.Concrete.Scoped.Name qualified as S

data ScopedModule = forall t. MkScopedModule (SModuleIsTop t) (Module 'Scoped t)

mkScopedModule :: forall t. SingI t => Module 'Scoped t -> ScopedModule
mkScopedModule = MkScopedModule sing

getAllModules :: Module 'Scoped 'ModuleTop -> HashMap S.NameId (Module 'Scoped 'ModuleTop)
getAllModules m =
  HashMap.fromList $ singl m : [singl n | Import n <- allImports (mkScopedModule m)]
  where
    allImports :: ScopedModule -> [Import 'Scoped]
    allImports (MkScopedModule _ w) =
      concat [i : allImports (mkScopedModule t) | StatementImport i@(Import t) <- w ^. moduleBody]
        <> concatMap (allImports . mkScopedModule) [l | StatementModule l <- w ^. moduleBody]

    singl :: Module 'Scoped 'ModuleTop -> (S.NameId, Module 'Scoped 'ModuleTop)
    singl n = (n ^. modulePath . S.nameId, n)

getModuleFilePath :: Module 'Scoped 'ModuleTop -> FilePath
getModuleFilePath m = getLoc (m ^. modulePath) ^. intFile
