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
  BuiltinIO -> Just io
  BuiltinIOSequence -> Just ioseq
  BuiltinString -> Just string_
  BuiltinStringPrint -> Just printString
  BuiltinBoolPrint -> Just printBool

builtinFunctionName :: BuiltinFunction -> Maybe Text
builtinFunctionName = \case
  BuiltinNatPlus -> Just natplus
  BuiltinBoolIf -> Just boolif

builtinName :: BuiltinPrim -> Maybe Text
builtinName = \case
  BuiltinsInductive i -> builtinInductiveName i
  BuiltinsConstructor i -> builtinConstructorName i
  BuiltinsAxiom i -> builtinAxiomName i
  BuiltinsFunction i -> builtinFunctionName i
