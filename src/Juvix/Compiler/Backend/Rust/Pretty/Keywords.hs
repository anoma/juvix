module Juvix.Compiler.Backend.Rust.Pretty.Keywords where

import Juvix.Data.CodeAnn
import Juvix.Extra.Strings qualified as Str

kwFn :: Doc Ann
kwFn = keyword Str.rustFn

kwIf :: Doc Ann
kwIf = keyword Str.rustIf

kwElse :: Doc Ann
kwElse = keyword Str.rustElse

kwMatch :: Doc Ann
kwMatch = keyword Str.rustMatch

kwLoop :: Doc Ann
kwLoop = keyword Str.rustLoop

kwConst :: Doc Ann
kwConst = keyword Str.rustConst

kwMut :: Doc Ann
kwMut = keyword Str.rustMut

kwVec :: Doc Ann
kwVec = keyword Str.rustVec

kwVector :: Doc Ann
kwVector = keyword Str.rustVector

kwWord :: Doc Ann
kwWord = keyword Str.rustWord

kwMemory :: Doc Ann
kwMemory = keyword Str.rustMemory

kwContinue :: Doc Ann
kwContinue = keyword Str.rustContinue

kwReturn :: Doc Ann
kwReturn = keyword Str.rustReturn
