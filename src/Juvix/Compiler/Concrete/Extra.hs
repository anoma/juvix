module Juvix.Compiler.Concrete.Extra
  ( module Juvix.Compiler.Concrete.Extra.Base,
    mkScopedModule,
    getAllModules,
    getModuleFilePath,
    unfoldApplication,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Extra.Base
import Juvix.Compiler.Concrete.Language
import Juvix.Prelude hiding (some)

data ScopedModule = forall t. MkScopedModule (SModuleIsTop t) (Module 'Scoped t)

mkScopedModule :: forall t. SingI t => Module 'Scoped t -> ScopedModule
mkScopedModule = MkScopedModule sing

getAllModules :: Module 'Scoped 'ModuleTop -> HashMap S.NameId (Module 'Scoped 'ModuleTop)
getAllModules m = HashMap.fromList (fst (run (runOutputList (getAllModules' m))))

getAllModules' ::
  forall r.
  Member (Output (S.NameId, Module 'Scoped 'ModuleTop)) r =>
  Module 'Scoped 'ModuleTop ->
  Sem r ()
getAllModules' m = recordModule m
  where
    recordModule :: Module 'Scoped 'ModuleTop -> Sem r ()
    recordModule n = do
      output (n ^. modulePath . S.nameId, n)
      processModule (mkScopedModule n)

    processModule :: ScopedModule -> Sem r ()
    processModule (MkScopedModule _ w) = forM_ (w ^. moduleBody) processStatement

    processStatement :: Statement 'Scoped -> Sem r ()
    processStatement = \case
      StatementImport i -> recordModule (i ^. importModule . moduleRefModule)
      StatementModule n -> processModule (mkScopedModule n)
      StatementOpenModule n -> forM_ (getModuleRefTopModule (n ^. openModuleName)) recordModule
      _ -> return ()

    getModuleRefTopModule :: ModuleRef' c -> Maybe (Module 'Scoped 'ModuleTop)
    getModuleRefTopModule (ModuleRef' (isTop :&: ModuleRef'' {..})) = case isTop of
      SModuleLocal -> Nothing
      SModuleTop -> Just _moduleRefModule

getModuleFilePath :: Module 'Scoped 'ModuleTop -> Path Abs File
getModuleFilePath m = getLoc (m ^. modulePath) ^. intervalFile

unfoldApplication :: Application -> (Expression, [Expression])
unfoldApplication (Application l r) = go [r] l
  where
    go :: [Expression] -> Expression -> (Expression, [Expression])
    go ac = \case
      ExpressionApplication (Application l' r') -> go (r' : ac) l'
      e -> (e, ac)
