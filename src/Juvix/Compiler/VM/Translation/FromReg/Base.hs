module Juvix.Compiler.VM.Translation.FromReg.Base where

import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Juvix.Compiler.Reg.Data.InfoTable qualified as Reg
import Juvix.Compiler.Reg.Extra qualified as Reg
import Juvix.Compiler.Reg.Language qualified as Reg
import Juvix.Prelude

mkIdent :: Text -> Text
mkIdent ident = T.filter isValidIdentChar ident

getFunctionName :: Reg.ExtraInfo -> Reg.Symbol -> Text
getFunctionName info sym = ((info ^. Reg.extraInfoTable . Reg.infoFunctions) HashMap.! sym) ^. Reg.functionName

-- Make sure this corresponds with juvix/object/object.h
getBuiltinUID :: Reg.BuiltinDataTag -> Int
getBuiltinUID = \case
  Reg.TagFalse -> 0
  Reg.TagTrue -> 1
  Reg.TagReturn -> 4
  Reg.TagBind -> 5
  Reg.TagWrite -> 6
  Reg.TagReadLn -> 7

getUID :: Reg.ExtraInfo -> Reg.Tag -> Int
getUID info tag = case tag of
  Reg.BuiltinTag builtin -> getBuiltinUID builtin
  Reg.UserTag {} -> fromJust $ HashMap.lookup tag (info ^. Reg.extraInfoUIDs)

getFUID :: Reg.ExtraInfo -> Reg.Symbol -> Int
getFUID info sym = fromJust $ HashMap.lookup sym (info ^. Reg.extraInfoFUIDs)

getLabel :: Reg.ExtraInfo -> Reg.Symbol -> Text
getLabel info sym = mkIdent $ "juvix_function_" <> getFunctionName info sym <> "_" <> show (getFUID info sym)
