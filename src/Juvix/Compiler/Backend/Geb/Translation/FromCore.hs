module Juvix.Compiler.Backend.Geb.Translation.FromCore where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Backend.Geb.Language
import Juvix.Compiler.Core.Data.InfoTable qualified as Core
import Juvix.Compiler.Core.Info.TypeInfo qualified as Info
import Juvix.Compiler.Core.Language qualified as Core
import Juvix.Prelude

{-
  TODO: The translation of each identifier should be saved separately to avoid
  exponential blow-up. For example, the program:
  ```
  a : A

  f : A -> A
  f x = F

  g : A -> A
  g x = f (f x)

  main : A
  main = g (g a)
  ```
  should be translated as:
  ```
  (\a -> (\f -> (\g -> g (g a)) (\x -> f (f x))) (\x -> F')) a'
  ```
-}

fromCore :: Core.InfoTable -> Core.Node -> Geb
fromCore tab = convertNode
  where
    unimplemented :: forall a. a
    unimplemented = error "not yet implemented"

    unsupported :: forall a. a
    unsupported = error "unsupported"

    convertNode :: Core.Node -> Geb
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

    convertVar :: Core.Var -> Geb
    convertVar Core.Var {} = unimplemented

    convertIdent :: Core.Ident -> Geb
    convertIdent Core.Ident {..} =
      -- TODO: unroll / check for recursion;
      -- TODO: memoize the result to avoid recomputing it each time a reference
      -- to the identifier is encountered
      convertNode node
      where
        node = fromJust $ HashMap.lookup _identSymbol (tab ^. Core.identContext)

    convertConstant :: Core.Constant -> Geb
    convertConstant Core.Constant {} = unsupported

    convertApp :: Core.App -> Geb
    convertApp Core.App {} = unimplemented

    convertBuiltinApp :: Core.BuiltinApp -> Geb
    convertBuiltinApp Core.BuiltinApp {} = unsupported

    convertConstr :: Core.Constr -> Geb
    convertConstr Core.Constr {} = unimplemented

    convertLet :: Core.Let -> Geb
    convertLet Core.Let {} = unimplemented

    convertLambda :: Core.Lambda -> Geb
    convertLambda Core.Lambda {..} =
      GebLamb
        Lamb
          { _lambVarType = convertType (_lambdaBinder ^. Core.binderType),
            _lambBodyType = convertType (Info.getNodeType _lambdaBody),
            _lambBody = convertNode _lambdaBody
          }

    convertCase :: Core.Case -> Geb
    convertCase Core.Case {} = unimplemented

    convertType :: Core.Type -> Obj
    convertType = \case
      Core.NPi x -> convertPi x
      Core.NUniv {} -> unsupported -- no polymorphism yet
      Core.NTyp x -> convertTypeConstr x
      Core.NPrim x -> convertTypePrim x
      Core.NDyn {} -> unsupported -- no dynamic type in GEB
      _ -> unsupported -- not a type
    convertPi :: Core.Pi -> Obj
    convertPi Core.Pi {..} =
      ObjHom
        Hom
          { _homDomain = convertType (_piBinder ^. Core.binderType),
            _homCodomain = convertType _piBody
          }

    convertTypeConstr :: Core.TypeConstr -> Obj
    convertTypeConstr Core.TypeConstr {} = unimplemented

    convertTypePrim :: Core.TypePrim -> Obj
    convertTypePrim Core.TypePrim {} = unimplemented
