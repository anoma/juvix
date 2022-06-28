module MiniJuvix.Translation.MonoJuvixToMiniC.BuiltinTable where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Builtins
import MiniJuvix.Translation.MonoJuvixToMiniC.CNames

builtinConstructorName :: BuiltinConstructor -> Maybe Text
builtinConstructorName = \case
  BuiltinNaturalZero -> Just zero
  BuiltinNaturalSuc -> Just suc

builtinInductiveName :: BuiltinInductive -> Maybe Text
builtinInductiveName = \case
  BuiltinNatural -> Just nat

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
