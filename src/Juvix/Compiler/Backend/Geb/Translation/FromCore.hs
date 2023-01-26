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
import Juvix.Compiler.Core.Pretty qualified as Core
import Juvix.Prelude

fromCore :: Core.InfoTable -> Geb
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
    goIdents :: HashMap Symbol Level -> Level -> [Level] -> Core.Node -> [Core.IdentifierInfo] -> Geb
    goIdents identMap level shiftLevels node = \case
      ii : idents ->
        GebApp
          App
            { _appDomainType = argty,
              _appCodomainType = nodeType,
              _appLeft = lamb,
              _appRight = convertNode identMap 0 shiftLevels fundef
            }
        where
          sym = ii ^. Core.identifierSymbol
          fundef = fromJust $ HashMap.lookup sym (tab ^. Core.identContext)
          argty = convertType (Info.getNodeType fundef)
          body = goIdents (HashMap.insert sym level identMap) (level + 1) (0 : shiftLevels) node idents
          lamb =
            GebLamb
              Lamb
                { _lambVarType = argty,
                  _lambBodyType = nodeType,
                  _lambBody = body
                }
      [] ->
        convertNode identMap 0 shiftLevels node
      where
        nodeType = convertType (Info.getNodeType node)

    -- `shiftLevels` contains the de Bruijn levels immediately before which a
    -- binder was inserted
    convertNode :: HashMap Symbol Level -> Level -> [Level] -> Core.Node -> Geb
    convertNode identMap varsNum shiftLevels node = trace (Core.ppTrace node) $ case node of
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

    convertVar :: HashMap Symbol Level -> Level -> [Level] -> Core.Var -> Geb
    convertVar _ varsNum shiftLevels Core.Var {..} =
      GebVar (_varIndex + insertedBinders varsNum shiftLevels _varIndex)

    convertIdent :: HashMap Symbol Level -> Level -> [Level] -> Core.Ident -> Geb
    convertIdent identMap varsNum shiftLevels Core.Ident {..} =
      GebVar (varsNum + length shiftLevels - fromJust (HashMap.lookup _identSymbol identMap) - 1)

    convertConstant :: HashMap Symbol Level -> Level -> [Level] -> Core.Constant -> Geb
    convertConstant _ _ _ Core.Constant {} = unsupported

    convertApp :: HashMap Symbol Level -> Level -> [Level] -> Core.App -> Geb
    convertApp identMap varsNum shiftLevels Core.App {..} =
      GebApp
        App
          { _appDomainType = convertType (Info.getNodeType _appRight),
            _appCodomainType = convertType (Info.getInfoType _appInfo),
            _appLeft = convertNode identMap varsNum shiftLevels _appLeft,
            _appRight = convertNode identMap varsNum shiftLevels _appRight
          }

    convertBuiltinApp :: HashMap Symbol Level -> Level -> [Level] -> Core.BuiltinApp -> Geb
    convertBuiltinApp _ _ _ Core.BuiltinApp {} = unsupported

    convertConstr :: HashMap Symbol Level -> Level -> [Level] -> Core.Constr -> Geb
    convertConstr identMap varsNum shiftLevels Core.Constr {..} =
      foldr ($) prod (replicate tagNum GebRight)
      where
        ci = fromJust $ HashMap.lookup _constrTag (tab ^. Core.infoConstructors)
        sym = ci ^. Core.constructorInductive
        ctrs = fromJust (HashMap.lookup sym (tab ^. Core.infoInductives)) ^. Core.inductiveConstructors
        tagNum = fromJust $ elemIndex _constrTag (sort (map (^. Core.constructorTag) ctrs))
        prod =
          (if tagNum == length ctrs - 1 then id else GebLeft)
            (convertProduct identMap varsNum shiftLevels _constrArgs)

    convertProduct :: HashMap Symbol Level -> Level -> [Level] -> [Core.Node] -> Geb
    convertProduct identMap varsNum shiftLevels = \case
      h : t ->
        fst $
          foldr
            (\x -> mkPair (convertNode identMap varsNum shiftLevels x, convertType (Info.getNodeType x)))
            (convertNode identMap varsNum shiftLevels h, convertType (Info.getNodeType h))
            t
      [] ->
        GebUnit
      where
        mkPair :: (Geb, Object) -> (Geb, Object) -> (Geb, Object)
        mkPair (x, xty) (y, yty) = (z, zty)
          where
            z =
              GebPair
                Pair
                  { _pairLeftType = xty,
                    _pairRightType = yty,
                    _pairLeft = x,
                    _pairRight = y
                  }
            zty = ObjectProd (Prod xty yty)

    convertLet :: HashMap Symbol Level -> Level -> [Level] -> Core.Let -> Geb
    convertLet identMap varsNum shiftLevels Core.Let {..} =
      GebApp
        App
          { _appCodomainType = domty,
            _appDomainType = codty,
            _appLeft =
              GebLamb
                Lamb
                  { _lambVarType = domty,
                    _lambBodyType = codty,
                    _lambBody = convertNode identMap varsNum shiftLevels _letBody
                  },
            _appRight = convertNode identMap varsNum shiftLevels (_letItem ^. Core.letItemValue)
          }
      where
        domty = convertType (_letItem ^. Core.letItemBinder . Core.binderType)
        codty = convertType (Info.getNodeType _letBody)

    convertLambda :: HashMap Symbol Level -> Level -> [Level] -> Core.Lambda -> Geb
    convertLambda identMap varsNum shiftLevels Core.Lambda {..} =
      GebLamb
        Lamb
          { _lambVarType = convertType (_lambdaBinder ^. Core.binderType),
            _lambBodyType = convertType (Info.getNodeType _lambdaBody),
            _lambBody = convertNode identMap (varsNum + 1) shiftLevels _lambdaBody
          }

    convertCase :: HashMap Symbol Level -> Level -> [Level] -> Core.Case -> Geb
    convertCase identMap varsNum shiftLevels Core.Case {..} =
      trace (show missingCtrsNum) $
        if
            | null branches ->
                GebAbsurd (convertNode identMap varsNum shiftLevels _caseValue)
            | missingCtrsNum > 1 ->
                let ty = convertType (Info.getNodeType defaultNode)
                 in GebApp
                      App
                        { _appDomainType = ty,
                          _appCodomainType = ty,
                          _appLeft =
                            GebLamb
                              Lamb
                                { _lambVarType = ty,
                                  _lambBodyType = ty,
                                  _lambBody = go indty (varsNum : shiftLevels) _caseValue branches
                                },
                          _appRight = convertNode identMap varsNum shiftLevels defaultNode
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

        go :: Object -> [Level] -> Core.Node -> [Core.CaseBranch] -> Geb
        go ty lvls val = \case
          [br] ->
            -- there is only one constructor, so `ty` is a product of its argument types
            mkBranch ty lvls val br
          br : brs ->
            GebCase
              Case
                { _caseLeftType = lty,
                  _caseRightType = rty,
                  _caseCodomainType = codty,
                  _caseOn = convertNode identMap varsNum lvls val,
                  _caseLeft =
                    GebLamb
                      Lamb
                        { _lambVarType = lty,
                          _lambBodyType = codty,
                          _lambBody = mkBranch lty (varsNum : lvls) val br
                        },
                  _caseRight =
                    GebLamb
                      Lamb
                        { _lambVarType = rty,
                          _lambBodyType = codty,
                          _lambBody = go rty (varsNum : lvls) (Core.mkVar' 0) brs
                        }
                }
            where
              (lty, rty) = case ty of
                ObjectCoprod Coprod {..} -> (_coprodLeft, _coprodRight)
                _ -> impossible
          [] -> impossible

        mkBranch :: Object -> [Level] -> Core.Node -> Core.CaseBranch -> Geb
        mkBranch valty lvls val Core.CaseBranch {..} =
          if
              | _caseBranchBindersNum == 0 ->
                  branch
              | otherwise ->
                  mkApps (mkLambs argtys) (convertNode identMap varsNum lvls val) valty argtys
          where
            branch = convertNode identMap (varsNum + _caseBranchBindersNum) lvls _caseBranchBody
            argtys = destructProd valty

            mkApps :: Geb -> Geb -> Object -> [Object] -> Geb
            mkApps acc v vty = \case
              ty : tys ->
                mkApps acc' v' rty tys
                where
                  v' =
                    GebSnd
                      Snd
                        { _sndLeftType = lty,
                          _sndRightType = rty,
                          _sndValue = v
                        }
                  acc' =
                    GebApp
                      App
                        { _appDomainType = ty,
                          _appCodomainType = vty,
                          _appLeft = acc,
                          _appRight =
                            if
                                | null tys ->
                                    v
                                | otherwise ->
                                    GebFst
                                      Fst
                                        { _fstLeftType = lty,
                                          _fstRightType = rty,
                                          _fstValue = v
                                        }
                        }
                  (lty, rty) = case vty of
                    ObjectProd Prod {..} -> (_prodLeft, _prodRight)
                    _ -> impossible
              [] ->
                acc

            mkLambs :: [Object] -> Geb
            mkLambs =
              fst
                . foldr
                  ( \ty (acc, accty) ->
                      ( GebLamb
                          Lamb
                            { _lambVarType = ty,
                              _lambBodyType = accty,
                              _lambBody = acc
                            },
                        ObjectHom (Hom ty accty)
                      )
                  )
                  (branch, codty)

    convertType :: Core.Type -> Object
    convertType ty = trace (Core.ppTrace ty) $ case ty of
      Core.NPi x ->
        convertPi x
      Core.NUniv {} ->
        unsupported -- no polymorphism yet
      Core.NTyp x ->
        convertTypeConstr x
      Core.NPrim x ->
        convertTypePrim x
      Core.NDyn {} ->
        unsupported -- no dynamic type in GEB
      Core.NLam Core.Lambda {..} ->
        convertType _lambdaBody
      _ ->
        unsupported -- not a type
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
        Core.PrimBool _ -> ObjectCoprod (Coprod ObjectTerminal ObjectTerminal)
        Core.PrimString -> unsupported

    convertInductive :: Symbol -> Object
    convertInductive sym =
      case reverse ctrs of
        ci : ctrs' ->
          foldr
            (\x acc -> ObjectCoprod (Coprod (convertConstructorType (x ^. Core.constructorType)) acc))
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
            (\x acc -> ObjectProd (Prod (convertType x) acc))
            (convertType hty)
            (reverse tys)
        [] ->
          ObjectTerminal
