module Juvix.Compiler.Backend.Geb.Keywords
  ( module Juvix.Compiler.Backend.Geb.Keywords,
    module Juvix.Data.Keyword,
    module Juvix.Data.Keyword.All,
  )
where

import Juvix.Data.Keyword
import Juvix.Data.Keyword.All
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude

allKeywordStrings :: HashSet Text
allKeywordStrings = keywordsStrings allKeywords

allKeywords :: [Keyword]
allKeywords =
  [ kwGebMorphismAbsurd,
    kwGebMorphismUnit,
    kwGebMorphismLeft,
    kwGebMorphismRight,
    kwGebMorphismCase,
    kwGebMorphismPair,
    kwGebMorphismFirst,
    kwGebMorphismSecond,
    kwGebMorphismLambda,
    kwGebMorphismApplication,
    kwGebObjectInteger,
    kwGebBinopAdd,
    kwGebBinopSub,
    kwGebBinopMul,
    kwGebBinopDiv,
    kwGebBinopMod,
    kwGebBinopEq,
    kwGebBinopLt,
    kwGebObjectInitial,
    kwGebObjectTerminal,
    kwGebObjectProduct,
    kwGebObjectCoproduct,
    kwGebObjectHom,
    kwGebVar
  ]

kwGebMorphismAbsurd :: Keyword
kwGebMorphismAbsurd = asciiKw Str.gebAbsurd

kwGebMorphismUnit :: Keyword
kwGebMorphismUnit = asciiKw Str.gebUnit

kwGebMorphismLeft :: Keyword
kwGebMorphismLeft = asciiKw Str.gebLeft

kwGebMorphismRight :: Keyword
kwGebMorphismRight = asciiKw Str.gebRight

kwGebMorphismCase :: Keyword
kwGebMorphismCase = asciiKw Str.gebCase

kwGebMorphismPair :: Keyword
kwGebMorphismPair = asciiKw Str.gebPair

kwGebMorphismFirst :: Keyword
kwGebMorphismFirst = asciiKw Str.gebFst

kwGebMorphismSecond :: Keyword
kwGebMorphismSecond = asciiKw Str.gebSnd

kwGebMorphismLambda :: Keyword
kwGebMorphismLambda = asciiKw Str.gebLamb

kwGebMorphismApplication :: Keyword
kwGebMorphismApplication = asciiKw Str.gebApp

kwGebBinopAdd :: Keyword
kwGebBinopAdd = asciiKw Str.gebAdd

kwGebBinopSub :: Keyword
kwGebBinopSub = asciiKw Str.gebSub

kwGebBinopMul :: Keyword
kwGebBinopMul = asciiKw Str.gebMul

kwGebBinopDiv :: Keyword
kwGebBinopDiv = asciiKw Str.gebDiv

kwGebBinopMod :: Keyword
kwGebBinopMod = asciiKw Str.gebMod

kwGebBinopEq :: Keyword
kwGebBinopEq = asciiKw Str.gebEq

kwGebBinopLt :: Keyword
kwGebBinopLt = asciiKw Str.gebLt

kwGebObjectInteger :: Keyword
kwGebObjectInteger = asciiKw Str.gebInteger

kwGebVar :: Keyword
kwGebVar = asciiKw Str.gebVar

kwGebObjectInitial :: Keyword
kwGebObjectInitial = asciiKw Str.gebInitial

kwGebObjectTerminal :: Keyword
kwGebObjectTerminal = asciiKw Str.gebTerminal

kwGebObjectProduct :: Keyword
kwGebObjectProduct = asciiKw Str.gebProd

kwGebObjectCoproduct :: Keyword
kwGebObjectCoproduct = asciiKw Str.gebCoprod

kwGebObjectHom :: Keyword
kwGebObjectHom = asciiKw Str.gebHom
