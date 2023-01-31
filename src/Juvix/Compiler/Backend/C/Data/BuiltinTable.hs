module Juvix.Compiler.Backend.C.Data.BuiltinTable where

import Juvix.Compiler.Backend.C.Data.CNames
import Juvix.Compiler.Concrete.Data.Builtins
import Juvix.Prelude

builtinConstructorName :: BuiltinConstructor -> Maybe Text
builtinConstructorName = \case
  BuiltinNatZero -> Just zero
  BuiltinNatSuc -> Just suc
  BuiltinBoolTrue -> Just true_
  BuiltinBoolFalse -> Just false_

builtinInductiveName :: BuiltinInductive -> Maybe Text
builtinInductiveName = \case
  BuiltinNat -> Just nat
  BuiltinBool -> Just bool_

builtinAxiomName :: BuiltinAxiom -> Maybe Text
builtinAxiomName = \case
  BuiltinNatPrint -> Just printNat
  BuiltinNatToString -> Just natToString
  BuiltinIO -> Just io
  BuiltinIOSequence -> Just ioseq
  BuiltinIOReadline -> Just ioreadline
  BuiltinString -> Just string_
  BuiltinStringPrint -> Just printString
  BuiltinStringConcat -> Just stringConcat
  BuiltinStringEq -> Just stringEq
  BuiltinStringToNat -> Just stringToNat
  BuiltinBoolPrint -> Just printBool
  BuiltinTrace -> Just trace_
  BuiltinFail -> Just fail_

builtinFunctionName :: BuiltinFunction -> Maybe Text
builtinFunctionName = \case
  BuiltinNatPlus -> Just natplus
  BuiltinNatSub -> Just natsub
  BuiltinNatMul -> Just natmul
  BuiltinNatUDiv -> Just natudiv
  BuiltinNatDiv -> Just natdiv
  BuiltinNatMod -> Just natmod
  BuiltinNatLe -> Just natle
  BuiltinNatLt -> Just natlt
  BuiltinNatEq -> Just nateq
  BuiltinBoolIf -> Just boolif
  BuiltinBoolOr -> Just boolor
  BuiltinBoolAnd -> Just booland

builtinName :: BuiltinPrim -> Maybe Text
builtinName = \case
  BuiltinsInductive i -> builtinInductiveName i
  BuiltinsConstructor i -> builtinConstructorName i
  BuiltinsAxiom i -> builtinAxiomName i
  BuiltinsFunction i -> builtinFunctionName i
