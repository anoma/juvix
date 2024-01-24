module Juvix.Compiler.Store.Core.Extra where

import Juvix.Compiler.Core.Data.InfoTable qualified as Core
import Juvix.Compiler.Core.Extra qualified as Core
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
    goIdentifierInfo IdentifierInfo {..} =
      Core.IdentifierInfo
        { _identifierType = goNode _identifierType,
          ..
        }

    goInductiveInfo :: InductiveInfo -> Core.InductiveInfo
    goInductiveInfo InductiveInfo {..} =
      Core.InductiveInfo
        { _inductiveKind = goNode _inductiveKind,
          _inductiveParams = map goParameterInfo _inductiveParams,
          ..
        }

    goParameterInfo :: ParameterInfo -> Core.ParameterInfo
    goParameterInfo ParameterInfo {..} =
      Core.ParameterInfo
        { _paramKind = goNode _paramKind,
          ..
        }

    goConstructorInfo :: ConstructorInfo -> Core.ConstructorInfo
    goConstructorInfo ConstructorInfo {..} =
      Core.ConstructorInfo
        { _constructorType = goNode _constructorType,
          ..
        }

    goAxiomInfo :: AxiomInfo -> Core.AxiomInfo
    goAxiomInfo AxiomInfo {..} =
      Core.AxiomInfo
        { _axiomType = goNode _axiomType,
          ..
        }

    goSpecialisationInfo :: SpecialisationInfo -> Core.SpecialisationInfo
    goSpecialisationInfo SpecialisationInfo {..} =
      Core.SpecialisationInfo
        { _specSignature = first (map goNode) _specSignature,
          ..
        }

    goNode :: Node -> Core.Node
    goNode = \case
      NVar Var {..} -> Core.mkVar' _varIndex
      NIdt Ident {..} -> Core.mkIdent' _identSymbol
      NCst Constant {..} -> Core.mkConstant' _constantValue
      NApp App {..} -> Core.mkApp' (goNode _appLeft) (goNode _appRight)
      NBlt BuiltinApp {..} -> Core.mkBuiltinApp' _builtinAppOp (map goNode _builtinAppArgs)
      NCtr Constr {..} -> Core.mkConstr' _constrTag (map goNode _constrArgs)
      NLam Lambda {..} -> Core.mkLambda mempty (goBinder _lambdaBinder) (goNode _lambdaBody)
      NLet Let {..} -> Core.NLet $ Core.Let mempty (goLetItem _letItem) (goNode _letBody)
      NRec LetRec {..} -> Core.NRec $ Core.LetRec mempty (fmap goLetItem _letRecValues) (goNode _letRecBody)
      NCase Case {..} -> Core.mkCase' _caseInductive (goNode _caseValue) (map goCaseBranch _caseBranches) (fmap goNode _caseDefault)
      NPi Pi {..} -> Core.mkPi mempty (goBinder _piBinder) (goNode _piBody)
      NUniv Univ {..} -> Core.mkUniv' _univLevel
      NTyp TypeConstr {..} -> Core.mkTypeConstr' _typeConstrSymbol (map goNode _typeConstrArgs)
      NPrim TypePrim {..} -> Core.mkTypePrim' _typePrimPrimitive
      NDyn Dynamic {} -> Core.mkDynamic'
      NBot Bottom {..} -> Core.mkBottom mempty (goNode _bottomType)

    goBinder :: Binder -> Core.Binder
    goBinder Binder {..} = Core.Binder _binderName _binderLocation (goNode _binderType)

    goLetItem :: LetItem -> Core.LetItem
    goLetItem LetItem {..} = Core.LetItem (goBinder _letItemBinder) (goNode _letItemValue)

    goCaseBranch :: CaseBranch -> Core.CaseBranch
    goCaseBranch CaseBranch {..} = Core.CaseBranch mempty _caseBranchTag (map goBinder _caseBranchBinders) _caseBranchBindersNum (goNode _caseBranchBody)

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
    goIdentifierInfo Core.IdentifierInfo {..} =
      IdentifierInfo
        { _identifierType = goNode _identifierType,
          ..
        }

    goInductiveInfo :: Core.InductiveInfo -> InductiveInfo
    goInductiveInfo Core.InductiveInfo {..} =
      InductiveInfo
        { _inductiveKind = goNode _inductiveKind,
          _inductiveParams = map goParameterInfo _inductiveParams,
          ..
        }

    goParameterInfo :: Core.ParameterInfo -> ParameterInfo
    goParameterInfo Core.ParameterInfo {..} =
      ParameterInfo
        { _paramKind = goNode _paramKind,
          ..
        }

    goConstructorInfo :: Core.ConstructorInfo -> ConstructorInfo
    goConstructorInfo Core.ConstructorInfo {..} =
      ConstructorInfo
        { _constructorType = goNode _constructorType,
          ..
        }

    goAxiomInfo :: Core.AxiomInfo -> AxiomInfo
    goAxiomInfo Core.AxiomInfo {..} =
      AxiomInfo
        { _axiomType = goNode _axiomType,
          ..
        }

    goSpecialisationInfo :: Core.SpecialisationInfo -> SpecialisationInfo
    goSpecialisationInfo Core.SpecialisationInfo {..} =
      SpecialisationInfo
        { _specSignature = first (map goNode) _specSignature,
          ..
        }

    goNode :: Core.Node -> Node
    goNode = \case
      Core.NVar Core.Var {..} -> NVar $ Var () _varIndex
      Core.NIdt Core.Ident {..} -> NIdt $ Ident () _identSymbol
      Core.NCst Core.Constant {..} -> NCst $ Constant () _constantValue
      Core.NApp Core.App {..} -> NApp $ App () (goNode _appLeft) (goNode _appRight)
      Core.NBlt Core.BuiltinApp {..} -> NBlt $ BuiltinApp () _builtinAppOp (map goNode _builtinAppArgs)
      Core.NCtr Core.Constr {..} -> NCtr $ Constr () _constrTag (map goNode _constrArgs)
      Core.NLam Core.Lambda {..} -> NLam $ Lambda () (goBinder _lambdaBinder) (goNode _lambdaBody)
      Core.NLet Core.Let {..} -> NLet $ Let () (goLetItem _letItem) (goNode _letBody)
      Core.NRec Core.LetRec {..} -> NRec $ LetRec () (fmap goLetItem _letRecValues) (goNode _letRecBody)
      Core.NCase Core.Case {..} -> NCase $ Case () _caseInductive (goNode _caseValue) (map goCaseBranch _caseBranches) (fmap goNode _caseDefault)
      Core.NPi Core.Pi {..} -> NPi $ Pi () (goBinder _piBinder) (goNode _piBody)
      Core.NUniv Core.Univ {..} -> NUniv $ Univ () _univLevel
      Core.NTyp Core.TypeConstr {..} -> NTyp $ TypeConstr () _typeConstrSymbol (map goNode _typeConstrArgs)
      Core.NPrim Core.TypePrim {..} -> NPrim $ TypePrim () _typePrimPrimitive
      Core.NDyn Core.Dynamic {} -> NDyn $ Dynamic ()
      Core.NBot Core.Bottom {..} -> NBot $ Bottom () (goNode _bottomType)
      Core.NMatch {} -> impossible
      Core.Closure {} -> impossible

    goBinder :: Core.Binder -> Binder
    goBinder Core.Binder {..} = Binder _binderName _binderLocation (goNode _binderType)

    goLetItem :: Core.LetItem -> LetItem
    goLetItem Core.LetItem {..} = LetItem (goBinder _letItemBinder) (goNode _letItemValue)

    goCaseBranch :: Core.CaseBranch -> CaseBranch
    goCaseBranch Core.CaseBranch {..} = CaseBranch mempty _caseBranchTag (map goBinder _caseBranchBinders) _caseBranchBindersNum (goNode _caseBranchBody)
