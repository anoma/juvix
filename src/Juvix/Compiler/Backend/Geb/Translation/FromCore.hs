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

data Env = Env
  { _envIdentMap :: HashMap Symbol Level,
    _envLevel :: Level,
    -- | `envShiftLevels` contains the de Bruijn levels immediately before which a
    -- | binder was inserted
    _envShiftLevels :: [Level]
  }

emptyEnv :: Env
emptyEnv =
  Env
    { _envIdentMap = mempty,
      _envLevel = 0,
      _envShiftLevels = []
    }

type Trans = Sem '[Reader Env]

makeLenses ''Env

zeroLevel :: Trans a -> Trans a
zeroLevel = local (set envLevel 0)

underBinders :: Int -> Trans a -> Trans a
underBinders n = local (over envLevel (+ n))

underBinder :: Trans a -> Trans a
underBinder = underBinders 1

shifting :: Trans a -> Trans a
shifting m = do
  varsNum <- asks (^. envLevel)
  local (over envShiftLevels (varsNum :)) m

withSymbol :: Symbol -> Trans a -> Trans a
withSymbol sym a = do
  level <- asks (^. envLevel)
  let modif :: Env -> Env =
        over envIdentMap (HashMap.insert sym level)
          . over envLevel (+ 1)
          . over envShiftLevels (0 :)
  local modif a

fromCore :: Core.InfoTable -> (Morphism, Object)
fromCore tab = case tab ^. Core.infoMain of
  Just sym ->
    let node = fromJust $ HashMap.lookup sym (tab ^. Core.identContext)
        idents = HashMap.delete sym (tab ^. Core.infoIdentifiers)
        morph = run (runReader emptyEnv (goIdents node (HashMap.elems idents)))
        obj = convertType (Info.getNodeType node)
     in (morph, obj)
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
    goIdents :: Core.Node -> [Core.IdentifierInfo] -> Trans Morphism
    goIdents node = \case
      [] ->
        zeroLevel (convertNode node)
      ii : idents -> do
        lamb <- mkLambda
        arg <- zeroLevel (convertNode fundef)
        return $
          MorphismApplication
            Application
              { _applicationDomainType = argty,
                _applicationCodomainType = nodeType,
                _applicationLeft = lamb,
                _applicationRight = arg
              }
        where
          sym = ii ^. Core.identifierSymbol
          fundef = fromJust $ HashMap.lookup sym (tab ^. Core.identContext)
          argty = convertType (Info.getNodeType fundef)
          mkLambda = do
            body <- withSymbol sym (goIdents node idents)
            return $
              MorphismLambda
                Lambda
                  { _lambdaVarType = argty,
                    _lambdaBodyType = nodeType,
                    _lambdaBody = body
                  }
      where
        nodeType = convertType (Info.getNodeType node)

    convertNode :: Core.Node -> Trans Morphism
    convertNode = \case
      Core.NVar x -> convertVar x
      Core.NIdt x -> convertIdent x
      Core.NCst x -> convertConstant x
      Core.NApp x -> convertApp x
      Core.NBlt x -> convertBuiltinApp x
      Core.NCtr x -> convertConstr x
      Core.NLam x -> convertLambda x
      Core.NLet x -> convertLet x
      Core.NRec {} -> unsupported -- LetRecs should be lifted out beforehand
      Core.NCase x -> convertCase x
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

    convertVar :: Core.Var -> Trans Morphism
    convertVar Core.Var {..} = do
      varsNum <- asks (^. envLevel)
      shiftLevels <- asks (^. envShiftLevels)
      return $ MorphismVar (Var (_varIndex + insertedBinders varsNum shiftLevels _varIndex))

    convertIdent :: Core.Ident -> Trans Morphism
    convertIdent Core.Ident {..} = do
      varsNum <- asks (^. envLevel)
      shiftLevels <- asks (^. envShiftLevels)
      identMap <- asks (^. envIdentMap)
      return $ MorphismVar (Var (varsNum + length shiftLevels - fromJust (HashMap.lookup _identSymbol identMap) - 1))

    convertConstant :: Core.Constant -> Trans Morphism
    convertConstant Core.Constant {..} = case _constantValue of
      Core.ConstInteger n -> return $ MorphismInteger n
      Core.ConstString {} -> unsupported

    convertApp :: Core.App -> Trans Morphism
    convertApp Core.App {..} = do
      _applicationLeft <- convertNode _appLeft
      _applicationRight <- convertNode _appRight
      return $
        MorphismApplication
          Application
            { _applicationDomainType = convertType (Info.getNodeType _appRight),
              _applicationCodomainType = convertType (Info.getInfoType _appInfo),
              _applicationLeft,
              _applicationRight
            }

    convertBuiltinApp :: Core.BuiltinApp -> Trans Morphism
    convertBuiltinApp Core.BuiltinApp {..} = case _builtinAppOp of
      Core.OpIntAdd ->
        convertBinop OpAdd _builtinAppArgs
      Core.OpIntSub ->
        convertBinop OpSub _builtinAppArgs
      Core.OpIntMul ->
        convertBinop OpMul _builtinAppArgs
      Core.OpIntDiv ->
        convertBinop OpDiv _builtinAppArgs
      Core.OpIntMod ->
        convertBinop OpMod _builtinAppArgs
      Core.OpIntLt ->
        convertBinop OpLt _builtinAppArgs
      Core.OpIntLe ->
        case _builtinAppArgs of
          [arg1, arg2] -> do
            arg1' <- convertNode arg1
            arg2' <- convertNode arg2
            let le =
                  MorphismLambda
                    Lambda
                      { _lambdaVarType = ObjectInteger,
                        _lambdaBodyType = ObjectHom (Hom ObjectInteger objectBool),
                        _lambdaBody =
                          MorphismLambda
                            Lambda
                              { _lambdaVarType = ObjectInteger,
                                _lambdaBodyType = objectBool,
                                _lambdaBody =
                                  mkOr
                                    (MorphismBinop $ Binop OpLt (MorphismVar (Var 1)) (MorphismVar (Var 0)))
                                    (MorphismBinop $ Binop OpEq (MorphismVar (Var 1)) (MorphismVar (Var 0)))
                              }
                      }
             in return $
                  MorphismApplication
                    Application
                      { _applicationDomainType = ObjectInteger,
                        _applicationCodomainType = ObjectHom (Hom ObjectInteger objectBool),
                        _applicationLeft =
                          MorphismApplication
                            Application
                              { _applicationDomainType = ObjectInteger,
                                _applicationCodomainType = objectBool,
                                _applicationLeft = le,
                                _applicationRight = arg2'
                              },
                        _applicationRight = arg1'
                      }
          _ ->
            error "wrong builtin application argument number"
      Core.OpEq ->
        case _builtinAppArgs of
          arg : _
            | Info.getNodeType arg == Core.mkTypeInteger' ->
                convertBinop OpEq _builtinAppArgs
          _ ->
            error "unsupported equality argument types"
      _ ->
        unsupported

    convertBinop :: Opcode -> [Core.Node] -> Trans Morphism
    convertBinop op args =
      case args of
        [arg1, arg2] -> do
          arg1' <- convertNode arg1
          arg2' <- convertNode arg2
          return $ MorphismBinop (Binop op arg1' arg2')
        _ ->
          error "wrong builtin application argument number"

    convertConstr :: Core.Constr -> Trans Morphism
    convertConstr Core.Constr {..} = do
      prod <- mkProd
      return (foldr ($) prod (replicate tagNum MorphismRight))
      where
        ci = fromJust $ HashMap.lookup _constrTag (tab ^. Core.infoConstructors)
        sym = ci ^. Core.constructorInductive
        ctrs = fromJust (HashMap.lookup sym (tab ^. Core.infoInductives)) ^. Core.inductiveConstructors
        tagNum = fromJust $ elemIndex _constrTag (sort (map (^. Core.constructorTag) ctrs))
        mkProd :: Trans Morphism
        mkProd =
          (if tagNum == length ctrs - 1 then id else MorphismLeft)
            <$> (convertProduct _constrArgs)

    convertProduct :: [Core.Node] -> Trans Morphism
    convertProduct args = do
      case reverse args of
        h : t -> do
          env <- ask
          let convertNode' = run . runReader env . convertNode
          return $
            fst $
              foldr
                (\x -> mkPair (convertNode' x, convertType (Info.getNodeType x)))
                (convertNode' h, convertType (Info.getNodeType h))
                (reverse t)
        [] -> return MorphismUnit
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

    convertLet :: Core.Let -> Trans Morphism
    convertLet Core.Let {..} = do
      _lambdaBody <- convertNode _letBody
      let domty = convertType (_letItem ^. Core.letItemBinder . Core.binderType)
          codty = convertType (Info.getNodeType _letBody)
      arg <- convertNode (_letItem ^. Core.letItemValue)
      return $
        MorphismApplication
          Application
            { _applicationCodomainType = domty,
              _applicationDomainType = codty,
              _applicationLeft =
                MorphismLambda
                  Lambda
                    { _lambdaVarType = domty,
                      _lambdaBodyType = codty,
                      _lambdaBody
                    },
              _applicationRight = arg
            }

    convertLambda :: Core.Lambda -> Trans Morphism
    convertLambda Core.Lambda {..} = do
      body <- underBinder (convertNode _lambdaBody)
      return $
        MorphismLambda
          Lambda
            { _lambdaVarType = convertType (_lambdaBinder ^. Core.binderType),
              _lambdaBodyType = convertType (Info.getNodeType _lambdaBody),
              _lambdaBody = body
            }

    convertCase :: Core.Case -> Trans Morphism
    convertCase Core.Case {..} = do
      if
          | null branches -> MorphismAbsurd <$> convertNode _caseValue
          | missingCtrsNum > 1 -> do
              arg <- convertNode defaultNode
              body <- shifting (go indty _caseValue branches)
              let ty = convertType (Info.getNodeType defaultNode)
              return $
                MorphismApplication
                  Application
                    { _applicationDomainType = ty,
                      _applicationCodomainType = ty,
                      _applicationLeft =
                        MorphismLambda
                          Lambda
                            { _lambdaVarType = ty,
                              _lambdaBodyType = ty,
                              _lambdaBody = body
                            },
                      _applicationRight = arg
                    }
          | otherwise -> go indty _caseValue branches
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

        go :: Object -> Core.Node -> [Core.CaseBranch] -> Trans Morphism
        go ty val = \case
          [br] ->
            -- there is only one constructor, so `ty` is a product of its argument types
            mkBranch ty val br
          br : brs -> do
            _caseOn <- convertNode val
            bodyLeft <- shifting (mkBranch lty val br)
            bodyRight <- shifting (go rty (Core.mkVar' 0) brs)
            return $
              MorphismCase
                Case
                  { _caseLeftType = lty,
                    _caseRightType = rty,
                    _caseCodomainType = codty,
                    _caseOn,
                    _caseLeft =
                      MorphismLambda
                        Lambda
                          { _lambdaVarType = lty,
                            _lambdaBodyType = codty,
                            _lambdaBody = bodyLeft
                          },
                    _caseRight =
                      MorphismLambda
                        Lambda
                          { _lambdaVarType = rty,
                            _lambdaBodyType = codty,
                            _lambdaBody = bodyRight
                          }
                  }
            where
              (lty, rty) = case ty of
                ObjectCoproduct Coproduct {..} -> (_coproductLeft, _coproductRight)
                _ -> impossible
          [] -> impossible

        mkBranch :: Object -> Core.Node -> Core.CaseBranch -> Trans Morphism
        mkBranch valty val Core.CaseBranch {..} = do
          branch <- underBinders _caseBranchBindersNum (convertNode _caseBranchBody)
          if
              | _caseBranchBindersNum == 0 -> return branch
              | otherwise -> do
                  val' <- convertNode val
                  return $ mkApps (mkLambs branch argtys) val' valty argtys
          where
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

            mkLambs :: Morphism -> [Object] -> Morphism
            mkLambs br =
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
                  (br, codty)

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
        Core.PrimInteger _ -> ObjectInteger
        Core.PrimBool _ -> objectBool
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
