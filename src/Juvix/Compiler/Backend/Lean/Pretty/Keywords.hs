module Juvix.Compiler.Backend.Lean.Pretty.Keywords where

import Juvix.Data.CodeAnn

-- | Lean-specific keywords
kwDefinition :: Doc Ann
kwDefinition = keyword "def"

kwTheorem :: Doc Ann
kwTheorem = keyword "theorem"

--kwAxiom :: Doc Ann
--kwAxiom = keyword "axiom"

kwDef :: Doc Ann
kwDef = keyword "def"

--kwInductive :: Doc Ann
--kwInductive = keyword "inductive"

kwStructure :: Doc Ann
kwStructure = keyword "structure"

kwInstance :: Doc Ann
kwInstance = keyword "instance"

kwClass :: Doc Ann
kwClass = keyword "class"

kwNamespace :: Doc Ann
kwNamespace = keyword "namespace"

kwSection :: Doc Ann
kwSection = keyword "section"

--kwImport :: Doc Ann
--kwImport = keyword "import"

--kwOpen :: Doc Ann
--kwOpen = keyword "open"

--kwLet :: Doc Ann
--kwLet = keyword "let"

--kwIn :: Doc Ann
--kwIn = keyword "in"

kwMatch :: Doc Ann
kwMatch = keyword "match"

kwWith :: Doc Ann
kwWith = keyword "with"

--kwIf :: Doc Ann
--kwIf = keyword "if"

kwThen :: Doc Ann
kwThen = keyword "then"

kwElse :: Doc Ann
kwElse = keyword "else"

kwWhere :: Doc Ann
kwWhere = keyword "where"

kwSort :: Doc Ann
kwSort = keyword "Sort"

kwLevel :: Doc Ann
kwLevel = keyword "Level"

kwPrelude :: Doc Ann
kwPrelude = keyword "prelude"

kwBase :: Doc Ann
kwBase = keyword "base"

kwExtends :: Doc Ann
kwExtends = keyword "extends"

kwField :: Doc Ann
kwField = keyword "field"

kwDefault :: Doc Ann
kwDefault = keyword "default"

kwQuote :: Doc Ann
kwQuote = keyword "quote"

kwArray :: Doc Ann
kwArray = keyword "Array"

kwProj :: Doc Ann
kwProj = keyword "proj"

kwRenaming :: Doc Ann
kwRenaming = keyword "renaming"

kwSetOption :: Doc Ann
kwSetOption = keyword "set_option"