module Juvix.Compiler.Backend.Isabelle.Pretty.Keywords where

import Juvix.Data.CodeAnn

kwDefinition :: Doc Ann
kwDefinition = keyword "definition"

kwFun :: Doc Ann
kwFun = keyword "fun"

kwDatatype :: Doc Ann
kwDatatype = keyword "datatype"

kwTypeSynonym :: Doc Ann
kwTypeSynonym = keyword "type_synonym"

kwUndefined :: Doc Ann
kwUndefined = keyword "undefined"

kwTheory :: Doc Ann
kwTheory = keyword "theory"

kwImports :: Doc Ann
kwImports = keyword "imports"

kwBegin :: Doc Ann
kwBegin = keyword "begin"
