module Juvix.Compiler.Backend.C.Data.BuiltinTable where

import Juvix.Compiler.Backend.C.Data.CNames
import Juvix.Compiler.Concrete.Data.Builtins
import Juvix.Prelude

builtinConstructorName :: BuiltinConstructor -> Maybe Text
builtinConstructorName = \case
  BuiltinNaturalZero -> Just zero
  BuiltinNaturalSuc -> Just suc
  BuiltinBooleanTrue -> Just true_
  BuiltinBooleanFalse -> Just false_

builtinInductiveName :: BuiltinInductive -> Maybe Text
builtinInductiveName = \case
  BuiltinNatural -> Just nat
  BuiltinBoolean -> Just bool_

builtinAxiomName :: BuiltinAxiom -> Maybe Text
builtinAxiomName = \case
  BuiltinNaturalPrint -> Just printNat
  BuiltinIO -> Just io
  BuiltinIOSequence -> Just ioseq

builtinFunctionName :: BuiltinFunction -> Maybe Text
builtinFunctionName = \case
  BuiltinNaturalPlus -> Just natplus

builtinName :: BuiltinPrim -> Maybe Text
builtinName = \case
  BuiltinsInductive i -> builtinInductiveName i
  BuiltinsConstructor i -> builtinConstructorName i
  BuiltinsAxiom i -> builtinAxiomName i
  BuiltinsFunction i -> builtinFunctionName i
