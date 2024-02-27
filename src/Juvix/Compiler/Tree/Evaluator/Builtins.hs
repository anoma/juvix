module Juvix.Compiler.Tree.Evaluator.Builtins where

import Juvix.Compiler.Tree.Data.InfoTable.Base
import Juvix.Compiler.Tree.Language.Builtins
import Juvix.Compiler.Tree.Language.Value
import Juvix.Compiler.Tree.Pretty.Base
import Juvix.Data.Field
import Juvix.Data.PPOutput
import Juvix.Prelude
import Text.Read qualified as T

type ErrorMsg = Text

evalBinop :: BinaryOp -> Value -> Value -> Either ErrorMsg Value
evalBinop op arg1 arg2 = case op of
  OpIntAdd -> goIntBinop (+) arg1 arg2
  OpIntSub -> goIntBinop (-) arg1 arg2
  OpIntMul -> goIntBinop (*) arg1 arg2
  OpIntDiv
    | arg2 == ValInteger 0 -> Left "division by zero"
    | otherwise -> goIntBinop quot arg1 arg2
  OpIntMod
    | arg2 == ValInteger 0 -> Left "division by zero"
    | otherwise -> goIntBinop rem arg1 arg2
  OpIntLe -> goIntCmpBinop (<=) arg1 arg2
  OpIntLt -> goIntCmpBinop (<) arg1 arg2
  OpFieldAdd -> goFieldBinop fieldAdd arg1 arg2
  OpFieldSub -> goFieldBinop fieldSub arg1 arg2
  OpFieldMul -> goFieldBinop fieldMul arg1 arg2
  OpFieldDiv -> case arg2 of
    ValField arg2'
      | fieldToInteger arg2' == 0 -> Left "division by zero"
    _ -> goFieldBinop fieldDiv arg1 arg2
  OpEq
    | arg1 == arg2 -> Right $ ValBool True
    | otherwise -> Right $ ValBool False
  OpStrConcat -> goStrConcat arg1 arg2
  where
    goIntBinop :: (Integer -> Integer -> Integer) -> Value -> Value -> Either ErrorMsg Value
    goIntBinop f v1 v2 = case (v1, v2) of
      (ValInteger i1, ValInteger i2) -> Right $ ValInteger (f i1 i2)
      _ -> Left "expected two integer arguments"

    goIntCmpBinop :: (Integer -> Integer -> Bool) -> Value -> Value -> Either ErrorMsg Value
    goIntCmpBinop f v1 v2 = case (v1, v2) of
      (ValInteger i1, ValInteger i2) -> Right $ ValBool (f i1 i2)
      _ -> Left "expected two integer arguments"

    goFieldBinop :: (FField -> FField -> FField) -> Value -> Value -> Either ErrorMsg Value
    goFieldBinop f v1 v2 = case (v1, v2) of
      (ValField i1, ValField i2) -> Right $ ValField (f i1 i2)
      _ -> Left "expected two field elements as arguments"

    goStrConcat :: Value -> Value -> Either ErrorMsg Value
    goStrConcat v1 v2 = case (v1, v2) of
      (ValString s1, ValString s2) -> Right $ ValString (s1 <> s2)
      _ -> Left "expected two string arguments"

evalUnop :: InfoTable' t e -> UnaryOp -> Value -> Either ErrorMsg Value
evalUnop tab op v = case op of
  OpShow -> Right $ ValString (printValue tab v)
  OpStrToInt -> goStringUnop strToInt v
  OpFieldToInt -> goFieldToInt v
  OpIntToField -> goIntToField v
  OpArgsNum -> goArgsNum v
  where
    strToInt :: Text -> Either ErrorMsg Value
    strToInt s = case T.readMaybe (fromText s) of
      Just i ->
        Right $ ValInteger i
      Nothing ->
        Left "string to integer: not an integer"

    goStringUnop :: (Text -> Either ErrorMsg Value) -> Value -> Either ErrorMsg Value
    goStringUnop f = \case
      ValString s -> f s
      _ -> Left "expected a string argument"

    goArgsNum :: Value -> Either ErrorMsg Value
    goArgsNum = \case
      ValClosure Closure {..} ->
        Right $ ValInteger (fromIntegral argsNum)
        where
          fi = lookupFunInfo tab _closureSymbol
          argsNum = fi ^. functionArgsNum - length _closureArgs
      _ ->
        Left "expected a closure"

    goFieldToInt :: Value -> Either ErrorMsg Value
    goFieldToInt = \case
      ValField f ->
        Right $ ValInteger $ fieldToInteger f
      _ ->
        Left "expected a field element"

    goIntToField :: Value -> Either ErrorMsg Value
    goIntToField = \case
      ValInteger i ->
        Right $ ValField $ fieldFromInteger (tab ^. infoFieldSize) i
      _ ->
        Left "expected an integer"

printValue :: InfoTable' t e -> Value -> Text
printValue tab = \case
  ValString s -> s
  v -> toPlainText . mkAnsiText . PPOutput . doc (defaultOptions tab) $ v
