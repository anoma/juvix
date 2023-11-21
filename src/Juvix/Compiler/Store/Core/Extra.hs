module Juvix.Compiler.Store.Core.Extra where

import Juvix.Compiler.Core.Data.InfoTable qualified as Core
import Juvix.Compiler.Core.Language qualified as Core
import Juvix.Compiler.Store.Core.Data.InfoTable
import Juvix.Compiler.Store.Core.Language

toCore :: InfoTable -> Core.InfoTable
toCore InfoTable {..} =
  Core.InfoTable
    { _identContext = fmap goNode _identContext,
      _identMap,
      _infoMain,
      _infoIdentifiers = fmap goIdentifierInfo _infoIdentifiers,
      _infoInductives = fmap goInductiveInfo _infoInductives,
      _infoConstructors = fmap goConstructorInfo _infoConstructors,
      _infoAxioms = fmap goAxiomInfo _infoAxioms,
      _infoSpecialisations = fmap (map goSpecialisationInfo) _infoSpecialisations,
      _infoLiteralIntToNat,
      _infoLiteralIntToInt,
      _infoBuiltins
    }
  where
    goIdentifierInfo :: IdentifierInfo -> Core.IdentifierInfo
    goIdentifierInfo = undefined

    goInductiveInfo :: InductiveInfo -> Core.InductiveInfo
    goInductiveInfo = undefined

    goConstructorInfo :: ConstructorInfo -> Core.ConstructorInfo
    goConstructorInfo = undefined

    goAxiomInfo :: AxiomInfo -> Core.AxiomInfo
    goAxiomInfo = undefined

    goSpecialisationInfo :: SpecialisationInfo -> Core.SpecialisationInfo
    goSpecialisationInfo = undefined

    goNode :: Node -> Core.Node
    goNode = \case {}

fromCore :: Core.InfoTable -> InfoTable
fromCore Core.InfoTable {..} =
  InfoTable
    { _identContext = fmap goNode _identContext,
      _identMap,
      _infoMain,
      _infoIdentifiers = fmap goIdentifierInfo _infoIdentifiers,
      _infoInductives = fmap goInductiveInfo _infoInductives,
      _infoConstructors = fmap goConstructorInfo _infoConstructors,
      _infoAxioms = fmap goAxiomInfo _infoAxioms,
      _infoSpecialisations = fmap (map goSpecialisationInfo) _infoSpecialisations,
      _infoLiteralIntToNat,
      _infoLiteralIntToInt,
      _infoBuiltins
    }
  where
    goIdentifierInfo :: Core.IdentifierInfo -> IdentifierInfo
    goIdentifierInfo = undefined

    goInductiveInfo :: Core.InductiveInfo -> InductiveInfo
    goInductiveInfo = undefined

    goConstructorInfo :: Core.ConstructorInfo -> ConstructorInfo
    goConstructorInfo = undefined

    goAxiomInfo :: Core.AxiomInfo -> AxiomInfo
    goAxiomInfo = undefined

    goSpecialisationInfo :: Core.SpecialisationInfo -> SpecialisationInfo
    goSpecialisationInfo = undefined

    goNode :: Core.Node -> Node
    goNode = \case {}
