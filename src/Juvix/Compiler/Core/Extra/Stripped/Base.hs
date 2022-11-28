module Juvix.Compiler.Core.Extra.Stripped.Base where

import Juvix.Compiler.Core.Language.Stripped

{------------------------------------------------------------------------}
{- Stripped Node constructors -}

mkVar :: VarInfo -> Index -> Node
mkVar i idx = NVar (Var i idx)

mkVar' :: Index -> Node
mkVar' = mkVar (VarInfo "" Nothing TyDynamic)

mkIdent :: IdentInfo -> Symbol -> Node
mkIdent i sym = NIdt (Ident i sym)

mkIdent' :: Symbol -> Node
mkIdent' = mkIdent (IdentInfo "" Nothing TyDynamic)

mkConstant :: ConstantValue -> Node
mkConstant cv = NCst (Constant () cv)

mkApps :: Fun -> [Node] -> Node
mkApps l r = NApp (Apps () l r)

mkBuiltinApp :: BuiltinOp -> [Node] -> Node
mkBuiltinApp op args = NBlt (BuiltinApp () op args)

mkConstr :: ConstrInfo -> Tag -> [Node] -> Node
mkConstr i tag args = NCtr (Constr i tag args)

mkConstr' :: Symbol -> Tag -> [Node] -> Node
mkConstr' sym = mkConstr (ConstrInfo "" Nothing TyDynamic sym)

mkLet :: LetInfo -> Node -> Node -> Node
mkLet i v b = NLet (Let i item b)
  where
    binder :: Binder
    binder =
      Binder
        { _binderName = i ^. letInfoBinderName,
          _binderLocation = i ^. letInfoBinderLocation,
          _binderType = i ^. letInfoBinderType
        }
    item :: LetItem
    item =
      LetItem
        { _letItemBinder = binder,
          _letItemValue = v
        }

mkLet' :: Node -> Node -> Node
mkLet' = mkLet (LetInfo "" Nothing TyDynamic)

mkCase :: CaseInfo -> Node -> [CaseBranch] -> Maybe Node -> Node
mkCase ci v bs def = NCase (Case ci v bs def)
