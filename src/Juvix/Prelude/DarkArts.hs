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
  return comp_id

qLookupPkgName :: String -> Q PkgName
qLookupPkgName pkg_name = do
  unit_id <- qLookupUnitId pkg_name
  return (PkgName (unitIdString unit_id))

importHiddenName :: (Name -> a) -> NameSpace -> String -> String -> String -> Q a
importHiddenName mkExp namespace pkgName modName valName = do
  pkgName' <- qLookupPkgName pkgName
  let name = Name (OccName valName) (NameG namespace pkgName' (ModName modName))
  return (mkExp name)

importHiddenConT :: String -> String -> String -> Q Type
importHiddenConT = importHiddenName ConT TcClsName

importHiddenCon :: String -> String -> String -> Q Exp
importHiddenCon = importHiddenName ConE DataName

importHidden :: String -> String -> String -> Q Exp
importHidden = importHiddenName VarE VarName

importHiddenField :: String -> String -> String -> String -> Q Exp
importHiddenField constructorName = importHiddenName VarE (FldName constructorName)
