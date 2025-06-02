module Juvix.Compiler.Backend.Rust.Translation.FromReg.Base where

import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Juvix.Compiler.Backend.Rust.Language
import Juvix.Compiler.Reg.Data.InfoTable qualified as Reg
import Juvix.Compiler.Reg.Extra.Info qualified as Reg
import Juvix.Compiler.Reg.Language qualified as Reg
import Juvix.Prelude

mkRustIdent :: Text -> Text
mkRustIdent ident = T.filter isValidIdentChar ident

getFunctionName :: Reg.ExtraInfo -> Reg.Symbol -> Text
getFunctionName info sym = ((info ^. Reg.extraInfoTable . Reg.infoFunctions) HashMap.! sym) ^. Reg.functionName

getBuiltinUID :: Reg.BuiltinDataTag -> Int
getBuiltinUID = \case
  Reg.TagFalse -> impossible
  Reg.TagTrue -> impossible
  Reg.TagReturn -> impossible
  Reg.TagBind -> impossible
  Reg.TagWrite -> impossible
  Reg.TagReadLn -> impossible
  Reg.TagJsonArray -> impossible
  Reg.TagJsonBool -> impossible
  Reg.TagJsonObject -> impossible
  Reg.TagJsonNumber -> impossible
  Reg.TagJsonString -> impossible

getUID :: Reg.ExtraInfo -> Reg.Tag -> Int
getUID info tag = case tag of
  Reg.BuiltinTag builtin -> getBuiltinUID builtin
  Reg.UserTag {} -> fromJust $ HashMap.lookup tag (info ^. Reg.extraInfoCIDs)

getFUID :: Reg.ExtraInfo -> Reg.Symbol -> Int
getFUID info sym = fromJust $ HashMap.lookup sym (info ^. Reg.extraInfoFUIDs)

getStringId :: Reg.ExtraInfo -> Text -> Int
getStringId info txt = fromJust $ HashMap.lookup txt (info ^. Reg.extraInfoStringMap)

getMaxStackHeight :: Reg.ExtraInfo -> Reg.Symbol -> Int
getMaxStackHeight info sym = fromJust $ HashMap.lookup sym (info ^. Reg.extraInfoMaxStackHeight)

getLocalVarsNum :: Reg.ExtraInfo -> Reg.Symbol -> Int
getLocalVarsNum info sym = fromJust $ HashMap.lookup sym (info ^. Reg.extraInfoLocalVarsNum)

getFunctionIdent :: Reg.ExtraInfo -> Reg.Symbol -> Text
getFunctionIdent info sym = mkRustIdent $ "JUVIX_FUNCTION_" <> T.toUpper (getFunctionName info sym) <> "_" <> show (getFUID info sym)

stmtAssign :: Text -> Expression -> Statement
stmtAssign result value = StatementAssignment $ Assignment result value

stmtsAssign :: Text -> Expression -> [Statement]
stmtsAssign result value = [stmtAssign result value]

stmtsCall :: Text -> [Expression] -> [Statement]
stmtsCall fun args = [StatementExpression $ mkCall fun args]

stmtsBlock :: [Statement] -> [Statement]
stmtsBlock stmts = [StatementExpression (ExprBlock (Block stmts))]

stmtLet :: IsMut -> Text -> Expression -> Statement
stmtLet isMut result value =
  StatementLet
    $ Let
      { _letMutable = isMut,
        _letVariable = result,
        _letType = Nothing,
        _letInitializer = Just value
      }

stmtDecl :: IsMut -> Text -> Type -> Statement
stmtDecl isMut var ty =
  StatementLet
    $ Let
      { _letMutable = isMut,
        _letVariable = var,
        _letType = Just ty,
        _letInitializer = Nothing
      }

stmtIf :: Expression -> [Statement] -> [Statement] -> Statement
stmtIf v br1 br2 = StatementIf $ If v br1 br2

stmtsIf :: Expression -> [Statement] -> [Statement] -> [Statement]
stmtsIf v br1 br2 = [stmtIf v br1 br2]

mkCall :: Text -> [Expression] -> Expression
mkCall fun args = ExprCall $ Call fun args

mkInteger :: (Integral a) => a -> Expression
mkInteger i = ExprLiteral $ LitInteger (fromIntegral i)

mkString :: Text -> Expression
mkString s = ExprLiteral $ LitString s

mkVar :: Text -> Expression
mkVar = ExprVar . Var

mkArray :: [Expression] -> Expression
mkArray = ExprArray . Array

mkVec :: [Expression] -> Expression
mkVec = ExprVec . Vec
