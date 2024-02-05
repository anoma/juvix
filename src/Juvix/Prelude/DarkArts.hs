module Juvix.Prelude.DarkArts where

import GHC.Data.FastString
import GHC.Driver.Env
import GHC.Tc.Types
import GHC.Tc.Utils.Monad
import GHC.Unit.State
import GHC.Unit.Types
import Juvix.Prelude.Base
import Language.Haskell.TH.Syntax
import Unsafe.Coerce

unsafeRunTcM :: TcM a -> Q a
unsafeRunTcM m = unsafeCoerce (const m)

qLookupUnitId :: String -> Q UnitId
qLookupUnitId pkg_name = do
  topEnv :: HscEnv <- unsafeRunTcM getTopEnv
  let uniState :: UnitState = hsc_units topEnv
  comp_id :: UnitId <- case lookupPackageName uniState (PackageName (fsLit pkg_name)) of
    Just comp_id -> pure comp_id
    _ -> error (pack ("Package not found: " ++ pkg_name))
  pure comp_id

qLookupPkgName :: String -> Q PkgName
qLookupPkgName pkg_name = do
  unit_id <- qLookupUnitId pkg_name
  pure (PkgName (unitIdString unit_id))

importHidden :: String -> String -> String -> Q Exp
importHidden = importHiddenName VarE

importHiddenName :: (Name -> Exp) -> String -> String -> String -> Q Exp
importHiddenName mkExp pkg_name mod_name val_name = do
  pkg_name' <- qLookupPkgName pkg_name
  pure $
    mkExp $
      Name
        (OccName val_name)
        (NameG VarName pkg_name' (ModName mod_name))

importHiddenCon :: String -> String -> String -> Q Exp
importHiddenCon = importHiddenName ConE
