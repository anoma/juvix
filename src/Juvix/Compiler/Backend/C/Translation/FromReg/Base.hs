module Juvix.Compiler.Backend.C.Translation.FromReg.Base where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Backend.C.Language
import Juvix.Compiler.Reg.Data.InfoTable qualified as Reg
import Juvix.Compiler.Reg.Extra qualified as Reg
import Juvix.Compiler.Reg.Language qualified as Reg
import Juvix.Prelude

getFunctionName :: Reg.ExtraInfo -> Reg.Symbol -> Text
getFunctionName info sym = ((info ^. Reg.extraInfoTable . Reg.infoFunctions) HashMap.! sym) ^. Reg.functionName

getUID :: Reg.ExtraInfo -> Reg.Tag -> Int
getUID info tag = fromJust $ HashMap.lookup tag (info ^. Reg.extraInfoUIDs)

getFUID :: Reg.ExtraInfo -> Reg.Symbol -> Int
getFUID info sym = fromJust $ HashMap.lookup sym (info ^. Reg.extraInfoFUIDs)

getMaxStackHeight :: Reg.ExtraInfo -> Reg.Symbol -> Int
getMaxStackHeight info sym = fromJust $ HashMap.lookup sym (info ^. Reg.extraInfoMaxStackHeight)

getLabel :: Reg.ExtraInfo -> Reg.Symbol -> Text
getLabel info sym = "juvix_function_" <> getFunctionName info sym <> "_" <> show (getFUID info sym)

getClosureLabel :: Reg.ExtraInfo -> Reg.Symbol -> Text
getClosureLabel info sym = "juvix_closure_" <> getFunctionName info sym <> "_" <> show (getFUID info sym)

exprAddr :: Reg.ExtraInfo -> Reg.Symbol -> Expression
exprAddr info sym = macroCall "LABEL_ADDR" [exprClosureLabel info sym]

exprLabel :: Reg.ExtraInfo -> Reg.Symbol -> Expression
exprLabel info sym = ExpressionVar $ getLabel info sym

exprClosureLabel :: Reg.ExtraInfo -> Reg.Symbol -> Expression
exprClosureLabel info sym = ExpressionVar $ getClosureLabel info sym

stmtAssign :: Expression -> Expression -> Statement
stmtAssign result value =
  StatementExpr $ ExpressionAssign (Assign result value)

stmtsAssign :: Expression -> Expression -> [Statement]
stmtsAssign result value = [stmtAssign result value]

stmtsPush :: [Expression] -> [Statement]
stmtsPush =
  map (\e -> StatementExpr $ macroCall "STACK_PUSH" [e])

stmtsPop :: [Expression] -> [Statement]
stmtsPop =
  map (\e -> StatementExpr $ macroCall "STACK_POP" [e])
    . reverse

stmtsCall :: Text -> [Expression] -> [Statement]
stmtsCall fun args = [StatementExpr $ macroCall fun args]
