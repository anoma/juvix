module Juvix.Compiler.Backend.Geb.Translation.FromCore where

import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Juvix.Compiler.Backend.Geb.Extra
import Juvix.Compiler.Backend.Geb.Language
import Juvix.Compiler.Core.Data.InfoTable qualified as Core
import Juvix.Compiler.Core.Extra qualified as Core
import Juvix.Compiler.Core.Info.TypeInfo qualified as Info
import Juvix.Compiler.Core.Language (Index, Level, Symbol)
import Juvix.Compiler.Core.Language qualified as Core

fromCore :: Core.InfoTable -> Morphism
fromCore tab = case tab ^. Core.infoMain of
  Just sym ->
    let node = fromJust $ HashMap.lookup sym (tab ^. Core.identContext)
        idents = HashMap.delete sym (tab ^. Core.infoIdentifiers)
     in goIdents mempty 0 [] node (HashMap.elems idents)
  Nothing ->
    error "no main function"
  where
    unsupported :: forall a. a
    unsupported = error "unsupported"

    {-
      The translation of each identifier is saved separately to avoid exponential
      blow-up. For example, the program:
      ```
      a : A

      f : A -> A
      f x = F

      g : A -> A
      g x = f (f x)

      main : A
      main = g (g a)
      ```
      is translated as if it were a single node:
      ```
      (\a -> (\f -> (\g -> g (g a)) (\x -> f (f x))) (\x -> F)) a
      ```
    -}
    goIdents :: HashMap Symbol Level -> Level -> [Level] -> Core.Node -> [Core.IdentifierInfo] -> Morphism
    goIdents identMap level shiftLevels node = \case
      ii : idents ->
        MorphismApplication
          Application
            { _applicationDomainType = argty,
              _applicationCodomainType = nodeType,
              _applicationLeft = lamb,
              _applicationRight = convertNode identMap 0 shiftLevels fundef
            }
        where
          sym = ii ^. Core.identifierSymbol
          fundef = fromJust $ HashMap.lookup sym (tab ^. Core.identContext)
          argty = convertType (Info.getNodeType fundef)
          body = goIdents (HashMap.insert sym level identMap) (level + 1) (0 : shiftLevels) node idents
          lamb =
            MorphismLambda
              Lambda
                { _lambdaVarType = argty,
                  _lambdaBodyType = nodeType,
                  _lambdaBody = body
                }
      [] ->
        convertNode identMap 0 shiftLevels node
      where
        nodeType = convertType (Info.getNodeType node)

    -- `shiftLevels` contains the de Bruijn levels immediately before which a
    -- binder was inserted
    convertNode :: HashMap Symbol Level -> Level -> [Level] -> Core.Node -> Morphism
    convertNode identMap varsNum shiftLevels = \case
      Core.NVar x -> convertVar identMap varsNum shiftLevels x
      Core.NIdt x -> convertIdent identMap varsNum shiftLevels x
      Core.NCst x -> convertConstant identMap varsNum shiftLevels x
      Core.NApp x -> convertApp identMap varsNum shiftLevels x
      Core.NBlt x -> convertBuiltinApp identMap varsNum shiftLevels x
      Core.NCtr x -> convertConstr identMap varsNum shiftLevels x
      Core.NLam x -> convertLambda identMap varsNum shiftLevels x
      Core.NLet x -> convertLet identMap varsNum shiftLevels x
      Core.NRec {} -> unsupported -- LetRecs should be lifted out beforehand
      Core.NCase x -> convertCase identMap varsNum shiftLevels x
      Core.NMatch {} -> unsupported -- Pattern matching should be compiled beforehand
      Core.NPi {} -> unsupported
      Core.NUniv {} -> unsupported
      Core.NTyp {} -> unsupported
      Core.NPrim {} -> unsupported
      Core.NDyn {} -> unsupported
      Core.Closure {} -> unsupported

    insertedBinders :: Level -> [Level] -> Index -> Int
    insertedBinders varsNum shiftLevels idx =
      length (filter ((varsNum - idx) <=) shiftLevels)

    convertVar :: HashMap Symbol Level -> Level -> [Level] -> Core.Var -> Morphism
    convertVar _ varsNum shiftLevels Core.Var {..} =
      MorphismVar (Var (_varIndex + insertedBinders varsNum shiftLevels _varIndex))

    convertIdent :: HashMap Symbol Level -> Level -> [Level] -> Core.Ident -> Morphism
    convertIdent identMap varsNum shiftLevels Core.Ident {..} =
      MorphismVar (Var (varsNum + length shiftLevels - fromJust (HashMap.lookup _identSymbol identMap) - 1))

    convertConstant :: HashMap Symbol Level -> Level -> [Level] -> Core.Constant -> Morphism
    convertConstant _ _ _ Core.Constant {} = unsupported

    convertApp :: HashMap Symbol Level -> Level -> [Level] -> Core.App -> Morphism
    convertApp identMap varsNum shiftLevels Core.App {..} =
      MorphismApplication
        Application
          { _applicationDomainType = convertType (Info.getNodeType _appRight),
            _applicationCodomainType = convertType (Info.getInfoType _appInfo),
            _applicationLeft = convertNode identMap varsNum shiftLevels _appLeft,
            _applicationRight = convertNode identMap varsNum shiftLevels _appRight
          }

    convertBuiltinApp :: HashMap Symbol Level -> Level -> [Level] -> Core.BuiltinApp -> Morphism
    convertBuiltinApp _ _ _ Core.BuiltinApp {} = unsupported

    convertConstr :: HashMap Symbol Level -> Level -> [Level] -> Core.Constr -> Morphism
    convertConstr identMap varsNum shiftLevels Core.Constr {..} =
      foldr ($) prod (replicate tagNum MorphismRight)
      where
        ci = fromJust $ HashMap.lookup _constrTag (tab ^. Core.infoConstructors)
        sym = ci ^. Core.constructorInductive
        ctrs = fromJust (HashMap.lookup sym (tab ^. Core.infoInductives)) ^. Core.inductiveConstructors
        tagNum = fromJust $ elemIndex _constrTag (sort (map (^. Core.constructorTag) ctrs))
        prod =
          (if tagNum == length ctrs - 1 then id else MorphismLeft)
            (convertProduct identMap varsNum shiftLevels _constrArgs)

    convertProduct :: HashMap Symbol Level -> Level -> [Level] -> [Core.Node] -> Morphism
    convertProduct identMap varsNum shiftLevels args = case reverse args of
      h : t ->
        fst $
          foldr
            (\x -> mkPair (convertNode identMap varsNum shiftLevels x, convertType (Info.getNodeType x)))
            (convertNode identMap varsNum shiftLevels h, convertType (Info.getNodeType h))
            (reverse t)
      [] ->
        MorphismUnit
      where
        mkPair :: (Morphism, Object) -> (Morphism, Object) -> (Morphism, Object)
        mkPair (x, xty) (y, yty) = (z, zty)
          where
            z =
              MorphismPair
                Pair
                  { _pairLeftType = xty,
                    _pairRightType = yty,
                    _pairLeft = x,
                    _pairRight = y
                  }
            zty = ObjectProduct (Product xty yty)

    convertLet :: HashMap Symbol Level -> Level -> [Level] -> Core.Let -> Morphism
    convertLet identMap varsNum shiftLevels Core.Let {..} =
      MorphismApplication
        Application
          { _applicationCodomainType = domty,
            _applicationDomainType = codty,
            _applicationLeft =
              MorphismLambda
                Lambda
                  { _lambdaVarType = domty,
                    _lambdaBodyType = codty,
                    _lambdaBody = convertNode identMap varsNum shiftLevels _letBody
                  },
            _applicationRight = convertNode identMap varsNum shiftLevels (_letItem ^. Core.letItemValue)
          }
      where
        domty = convertType (_letItem ^. Core.letItemBinder . Core.binderType)
        codty = convertType (Info.getNodeType _letBody)

    convertLambda :: HashMap Symbol Level -> Level -> [Level] -> Core.Lambda -> Morphism
    convertLambda identMap varsNum shiftLevels Core.Lambda {..} =
      MorphismLambda
        Lambda
          { _lambdaVarType = convertType (_lambdaBinder ^. Core.binderType),
            _lambdaBodyType = convertType (Info.getNodeType _lambdaBody),
            _lambdaBody = convertNode identMap (varsNum + 1) shiftLevels _lambdaBody
          }

    convertCase :: HashMap Symbol Level -> Level -> [Level] -> Core.Case -> Morphism
    convertCase identMap varsNum shiftLevels Core.Case {..} =
      if
          | null branches ->
              MorphismAbsurd (convertNode identMap varsNum shiftLevels _caseValue)
          | missingCtrsNum > 1 ->
              let ty = convertType (Info.getNodeType defaultNode)
               in MorphismApplication
                    Application
                      { _applicationDomainType = ty,
                        _applicationCodomainType = ty,
                        _applicationLeft =
                          MorphismLambda
                            Lambda
                              { _lambdaVarType = ty,
                                _lambdaBodyType = ty,
                                _lambdaBody = go indty (varsNum : shiftLevels) _caseValue branches
                              },
                        _applicationRight = convertNode identMap varsNum shiftLevels defaultNode
                      }
          | otherwise -> go indty shiftLevels _caseValue branches
      where
        indty = convertInductive _caseInductive
        ii = fromJust $ HashMap.lookup _caseInductive (tab ^. Core.infoInductives)
        missingCtrs =
          filter
            ( \x ->
                isNothing (find (\y -> x ^. Core.constructorTag == y ^. Core.caseBranchTag) _caseBranches)
            )
            (ii ^. Core.inductiveConstructors)
        missingCtrsNum = length missingCtrs
        ctrBrs = map mkCtrBranch missingCtrs
        defaultNode = fromMaybe (error "not all cases covered") _caseDefault
        -- `branches` contains one branch for each constructor of the inductive type.
        -- `_caseDefault` is the body of those branches which were not present in
        -- `_caseBranches`.
        branches = sortOn (^. Core.caseBranchTag) (_caseBranches ++ ctrBrs)
        codty = convertType (Info.getNodeType (List.head branches ^. Core.caseBranchBody))

        mkCtrBranch :: Core.ConstructorInfo -> Core.CaseBranch
        mkCtrBranch ci =
          Core.CaseBranch
            { _caseBranchInfo = mempty,
              _caseBranchTag = ci ^. Core.constructorTag,
              _caseBranchBinders = map (Core.Binder "?" Nothing) tyargs,
              _caseBranchBindersNum = n,
              _caseBranchBody = defaultBody n
            }
          where
            tyargs = Core.typeArgs (ci ^. Core.constructorType)
            n = length tyargs
            defaultBody =
              if
                  | missingCtrsNum > 1 -> Core.mkVar'
                  | otherwise -> (`Core.shift` defaultNode)

        go :: Object -> [Level] -> Core.Node -> [Core.CaseBranch] -> Morphism
        go ty lvls val = \case
          [br] ->
            -- there is only one constructor, so `ty` is a product of its argument types
            mkBranch ty lvls val br
          br : brs ->
            MorphismCase
              Case
                { _caseLeftType = lty,
                  _caseRightType = rty,
                  _caseCodomainType = codty,
                  _caseOn = convertNode identMap varsNum lvls val,
                  _caseLeft =
                    MorphismLambda
                      Lambda
                        { _lambdaVarType = lty,
                          _lambdaBodyType = codty,
                          _lambdaBody = mkBranch lty (varsNum : lvls) val br
                        },
                  _caseRight =
                    MorphismLambda
                      Lambda
                        { _lambdaVarType = rty,
                          _lambdaBodyType = codty,
                          _lambdaBody = go rty (varsNum : lvls) (Core.mkVar' 0) brs
                        }
                }
            where
              (lty, rty) = case ty of
                ObjectCoproduct Coproduct {..} -> (_coproductLeft, _coproductRight)
                _ -> impossible
          [] -> impossible

        mkBranch :: Object -> [Level] -> Core.Node -> Core.CaseBranch -> Morphism
        mkBranch valty lvls val Core.CaseBranch {..} =
          if
              | _caseBranchBindersNum == 0 ->
                  branch
              | otherwise ->
                  mkApps (mkLambs argtys) (convertNode identMap varsNum lvls val) valty argtys
          where
            branch = convertNode identMap (varsNum + _caseBranchBindersNum) lvls _caseBranchBody
            argtys = destructProduct valty

            mkApps :: Morphism -> Morphism -> Object -> [Object] -> Morphism
            mkApps acc v vty = \case
              ty : tys ->
                mkApps acc' v' rty tys
                where
                  v' =
                    MorphismSecond
                      Second
                        { _secondLeftType = lty,
                          _secondRightType = rty,
                          _secondValue = v
                        }
                  acc' =
                    MorphismApplication
                      Application
                        { _applicationDomainType = ty,
                          _applicationCodomainType = vty,
                          _applicationLeft = acc,
                          _applicationRight =
                            if
                                | null tys ->
                                    v
                                | otherwise ->
                                    MorphismFirst
                                      First
                                        { _firstLeftType = lty,
                                          _firstRightType = rty,
                                          _firstValue = v
                                        }
                        }
                  (lty, rty) = case vty of
                    ObjectProduct Product {..} -> (_productLeft, _productRight)
                    _ -> impossible
              [] ->
                acc

            mkLambs :: [Object] -> Morphism
            mkLambs =
              fst
                . foldr
                  ( \ty (acc, accty) ->
                      ( MorphismLambda
                          Lambda
                            { _lambdaVarType = ty,
                              _lambdaBodyType = accty,
                              _lambdaBody = acc
                            },
                        ObjectHom (Hom ty accty)
                      )
                  )
                  (branch, codty)

    convertType :: Core.Type -> Object
    convertType = \case
      Core.NPi x ->
        convertPi x
      Core.NUniv {} ->
        unsupported -- no polymorphism yet
      Core.NTyp x ->
        convertTypeConstr x
      Core.NPrim x ->
        convertTypePrim x
      Core.NDyn {} ->
        error "incomplete type information (dynamic type encountered)"
      Core.NLam Core.Lambda {..} ->
        convertType _lambdaBody
      _ ->
        unsupported

    convertPi :: Core.Pi -> Object
    convertPi Core.Pi {..} =
      ObjectHom
        Hom
          { _homDomain = convertType (_piBinder ^. Core.binderType),
            _homCodomain = convertType _piBody
          }

    convertTypeConstr :: Core.TypeConstr -> Object
    convertTypeConstr Core.TypeConstr {..} = convertInductive _typeConstrSymbol

    convertTypePrim :: Core.TypePrim -> Object
    convertTypePrim Core.TypePrim {..} =
      case _typePrimPrimitive of
        Core.PrimInteger _ -> unsupported
        Core.PrimBool _ -> ObjectCoproduct (Coproduct ObjectTerminal ObjectTerminal)
        Core.PrimString -> unsupported

    convertInductive :: Symbol -> Object
    convertInductive sym =
      case reverse ctrs of
        ci : ctrs' ->
          foldr
            (\x acc -> ObjectCoproduct (Coproduct (convertConstructorType (x ^. Core.constructorType)) acc))
            (convertConstructorType (ci ^. Core.constructorType))
            (reverse ctrs')
        [] ->
          ObjectInitial
      where
        ctrs =
          sortOn (^. Core.constructorTag) $
            fromJust (HashMap.lookup sym (tab ^. Core.infoInductives)) ^. Core.inductiveConstructors

    convertConstructorType :: Core.Node -> Object
    convertConstructorType ty =
      case reverse (Core.typeArgs ty) of
        hty : tys ->
          foldr
            (\x acc -> ObjectProduct (Product (convertType x) acc))
            (convertType hty)
            (reverse tys)
        [] ->
          ObjectTerminal
