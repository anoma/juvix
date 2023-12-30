module Juvix.Compiler.Store.Core.Data.InfoTable
  ( module Juvix.Compiler.Store.Core.Data.InfoTable,
    module Juvix.Compiler.Core.Data.InfoTable.Base,
  )
where

import Juvix.Compiler.Core.Data.InfoTable.Base
import Juvix.Compiler.Store.Core.Language

type InfoTable = InfoTable' Node

type IdentifierInfo = IdentifierInfo' Node

type InductiveInfo = InductiveInfo' Node

type ConstructorInfo = ConstructorInfo' Node

type AxiomInfo = AxiomInfo' Node

type ParameterInfo = ParameterInfo' Node

type SpecialisationInfo = SpecialisationInfo' Node
