module Juvix.Compiler.Store.Core.Extra where

import Juvix.Compiler.Core.Data.InfoTable qualified as Core
import Juvix.Compiler.Core.Extra qualified as Core
import Juvix.Compiler.Core.Info.PragmaInfo
import Juvix.Compiler.Store.Core.Data.InfoTable
import Juvix.Compiler.Store.Core.Language

toCore :: InfoTable -> Core.InfoTable
toCore InfoTable {..} =
  Core.InfoTable
    { _identContext = fmap toCoreNode _identContext,
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
        { _identifierType = toCoreNode _identifierType,
          ..
        }

    goInductiveInfo :: InductiveInfo -> Core.InductiveInfo
    goInductiveInfo InductiveInfo {..} =
      Core.InductiveInfo
        { _inductiveKind = toCoreNode _inductiveKind,
          _inductiveParams = map goParameterInfo _inductiveParams,
          ..
        }

    goParameterInfo :: ParameterInfo -> Core.ParameterInfo
    goParameterInfo ParameterInfo {..} =
      Core.ParameterInfo
        { _paramKind = toCoreNode _paramKind,
          ..
        }

    goConstructorInfo :: ConstructorInfo -> Core.ConstructorInfo
    goConstructorInfo ConstructorInfo {..} =
      Core.ConstructorInfo
        { _constructorType = toCoreNode _constructorType,
          ..
        }

    goAxiomInfo :: AxiomInfo -> Core.AxiomInfo
    goAxiomInfo AxiomInfo {..} =
      Core.AxiomInfo
        { _axiomType = toCoreNode _axiomType,
          ..
        }

    goSpecialisationInfo :: SpecialisationInfo -> Core.SpecialisationInfo
    goSpecialisationInfo SpecialisationInfo {..} =
      Core.SpecialisationInfo
        { _specSignature = first (map toCoreNode) _specSignature,
          ..
        }

toCoreNode :: Node -> Core.Node
toCoreNode = \case
  NVar Var {..} -> Core.mkVar' _varIndex
  NIdt Ident {..} -> Core.mkIdent' _identSymbol
  NCst Constant {..} -> Core.mkConstant' _constantValue
  NApp App {..} -> Core.mkApp' (toCoreNode _appLeft) (toCoreNode _appRight)
  NBlt BuiltinApp {..} -> Core.mkBuiltinApp' _builtinAppOp (map toCoreNode _builtinAppArgs)
  NCtr Constr {..} -> Core.mkConstr' _constrTag (map toCoreNode _constrArgs)
  NLam Lambda {..} -> Core.mkLambda (setInfoPragma (_lambdaInfo ^. lambdaInfoPragma) mempty) (goBinder _lambdaBinder) (toCoreNode _lambdaBody)
  NLet Let {..} -> Core.NLet $ Core.Let mempty (goLetItem _letItem) (toCoreNode _letBody)
  NRec LetRec {..} -> Core.NRec $ Core.LetRec (setInfoPragmas (_letRecInfo ^. letRecInfoPragmas) mempty) (fmap goLetItem _letRecValues) (toCoreNode _letRecBody)
  NCase Case {..} -> Core.mkCase' _caseInductive (toCoreNode _caseValue) (map goCaseBranch _caseBranches) (fmap toCoreNode _caseDefault)
  NPi Pi {..} -> Core.mkPi mempty (goBinder _piBinder) (toCoreNode _piBody)
  NUniv Univ {..} -> Core.mkUniv' _univLevel
  NTyp TypeConstr {..} -> Core.mkTypeConstr' _typeConstrSymbol (map toCoreNode _typeConstrArgs)
  NPrim TypePrim {..} -> Core.mkTypePrim' _typePrimPrimitive
  NDyn DynamicTy {} -> Core.mkDynamic'
  NBot Bottom {..} -> Core.mkBottom mempty (toCoreNode _bottomType)
  where
    goBinder :: Binder -> Core.Binder
    goBinder Binder {..} = Core.Binder _binderName _binderLocation (toCoreNode _binderType)

    goLetItem :: LetItem -> Core.LetItem
    goLetItem LetItem {..} = Core.LetItem (goBinder _letItemBinder) (toCoreNode _letItemValue)

    goCaseBranch :: CaseBranch -> Core.CaseBranch
    goCaseBranch CaseBranch {..} = Core.CaseBranch mempty _caseBranchTag (map goBinder _caseBranchBinders) _caseBranchBindersNum (toCoreNode _caseBranchBody)

fromCore :: Core.InfoTable -> InfoTable
fromCore Core.InfoTable {..} =
  InfoTable
    { _identContext = fmap fromCoreNode _identContext,
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
        { _identifierType = fromCoreNode _identifierType,
          ..
        }

    goInductiveInfo :: Core.InductiveInfo -> InductiveInfo
    goInductiveInfo Core.InductiveInfo {..} =
      InductiveInfo
        { _inductiveKind = fromCoreNode _inductiveKind,
          _inductiveParams = map goParameterInfo _inductiveParams,
          ..
        }

    goParameterInfo :: Core.ParameterInfo -> ParameterInfo
    goParameterInfo Core.ParameterInfo {..} =
      ParameterInfo
        { _paramKind = fromCoreNode _paramKind,
          ..
        }

    goConstructorInfo :: Core.ConstructorInfo -> ConstructorInfo
    goConstructorInfo Core.ConstructorInfo {..} =
      ConstructorInfo
        { _constructorType = fromCoreNode _constructorType,
          ..
        }

    goAxiomInfo :: Core.AxiomInfo -> AxiomInfo
    goAxiomInfo Core.AxiomInfo {..} =
      AxiomInfo
        { _axiomType = fromCoreNode _axiomType,
          ..
        }

    goSpecialisationInfo :: Core.SpecialisationInfo -> SpecialisationInfo
    goSpecialisationInfo Core.SpecialisationInfo {..} =
      SpecialisationInfo
        { _specSignature = first (map fromCoreNode) _specSignature,
          ..
        }

fromCoreNode :: Core.Node -> Node
fromCoreNode = \case
  Core.NVar Core.Var {..} -> NVar $ Var () _varIndex
  Core.NIdt Core.Ident {..} -> NIdt $ Ident () _identSymbol
  Core.NCst Core.Constant {..} -> NCst $ Constant () _constantValue
  Core.NApp Core.App {..} -> NApp $ App () (fromCoreNode _appLeft) (fromCoreNode _appRight)
  Core.NBlt Core.BuiltinApp {..} -> NBlt $ BuiltinApp () _builtinAppOp (map fromCoreNode _builtinAppArgs)
  Core.NCtr Core.Constr {..} -> NCtr $ Constr () _constrTag (map fromCoreNode _constrArgs)
  Core.NLam Core.Lambda {..} -> NLam $ Lambda (LambdaInfo (getInfoPragma _lambdaInfo)) (goBinder _lambdaBinder) (fromCoreNode _lambdaBody)
  Core.NLet Core.Let {..} -> NLet $ Let () (goLetItem _letItem) (fromCoreNode _letBody)
  Core.NRec Core.LetRec {..} -> NRec $ LetRec (LetRecInfo (getInfoPragmas _letRecInfo)) (fmap goLetItem _letRecValues) (fromCoreNode _letRecBody)
  Core.NCase Core.Case {..} -> NCase $ Case () _caseInductive (fromCoreNode _caseValue) (map goCaseBranch _caseBranches) (fmap fromCoreNode _caseDefault)
  Core.NPi Core.Pi {..} -> NPi $ Pi () (goBinder _piBinder) (fromCoreNode _piBody)
  Core.NUniv Core.Univ {..} -> NUniv $ Univ () _univLevel
  Core.NTyp Core.TypeConstr {..} -> NTyp $ TypeConstr () _typeConstrSymbol (map fromCoreNode _typeConstrArgs)
  Core.NPrim Core.TypePrim {..} -> NPrim $ TypePrim () _typePrimPrimitive
  Core.NDyn Core.DynamicTy {} -> NDyn $ DynamicTy ()
  Core.NBot Core.Bottom {..} -> NBot $ Bottom () (fromCoreNode _bottomType)
  Core.NMatch {} -> impossible
  Core.Closure {} -> impossible
  where
    goBinder :: Core.Binder -> Binder
    goBinder Core.Binder {..} = Binder _binderName _binderLocation (fromCoreNode _binderType)

    goLetItem :: Core.LetItem -> LetItem
    goLetItem Core.LetItem {..} = LetItem (goBinder _letItemBinder) (fromCoreNode _letItemValue)

    goCaseBranch :: Core.CaseBranch -> CaseBranch
    goCaseBranch Core.CaseBranch {..} = CaseBranch mempty _caseBranchTag (map goBinder _caseBranchBinders) _caseBranchBindersNum (fromCoreNode _caseBranchBody)
