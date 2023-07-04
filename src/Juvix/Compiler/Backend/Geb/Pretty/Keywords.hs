module Juvix.Compiler.Backend.Geb.Pretty.Keywords where

import Juvix.Compiler.Backend.Geb.Language
import Juvix.Data.CodeAnn
import Juvix.Extra.Strings qualified as Str

kwAbsurd :: Doc Ann
kwAbsurd = keyword Str.gebAbsurd

kwUnit :: Doc Ann
kwUnit = keyword Str.gebUnit

kwLeft :: Doc Ann
kwLeft = keyword Str.gebLeft

kwRight :: Doc Ann
kwRight = keyword Str.gebRight

kwFst :: Doc Ann
kwFst = keyword Str.gebFst

kwSnd :: Doc Ann
kwSnd = keyword Str.gebSnd

kwPair :: Doc Ann
kwPair = keyword Str.gebPair

kwLamb :: Doc Ann
kwLamb = keyword Str.gebLamb

kwList :: Doc Ann
kwList = keyword Str.gebList

kwClosure :: Doc Ann
kwClosure = keyword Str.gebValueClosure

kwClosureEnv :: Doc Ann
kwClosureEnv = keyword Str.gebValueClosureEnv

kwNil :: Doc Ann
kwNil = keyword Str.lispNil

kwApp :: Doc Ann
kwApp = keyword Str.gebApp

kwVar :: Doc Ann
kwVar = keyword Str.gebVar

kwAdd :: Doc Ann
kwAdd = keyword Str.gebAdd

kwSub :: Doc Ann
kwSub = keyword Str.gebSub

kwMul :: Doc Ann
kwMul = keyword Str.gebMul

kwDiv :: Doc Ann
kwDiv = keyword Str.gebDiv

kwMod :: Doc Ann
kwMod = keyword Str.gebMod

kwEq :: Doc Ann
kwEq = keyword Str.gebEq

kwLt :: Doc Ann
kwLt = keyword Str.gebLt

kwInitial :: Doc Ann
kwInitial = keyword Str.gebInitial

kwTerminal :: Doc Ann
kwTerminal = keyword Str.gebTerminal

kwProd :: Doc Ann
kwProd = keyword Str.gebProd

kwCoprod :: Doc Ann
kwCoprod = keyword Str.gebCoprod

kwHom :: Doc Ann
kwHom = keyword Str.gebHom

kwInteger :: Doc Ann
kwInteger = keyword Str.gebInteger

kwTyped :: Doc Ann
kwTyped = keyword Str.gebTyped

kwFail :: Doc Ann
kwFail = keyword Str.gebFail
