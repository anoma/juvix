module Juvix.Compiler.Core.Extra.Stripped.Base where

import Juvix.Compiler.Core.Language.Stripped

{------------------------------------------------------------------------}
{- Stripped Node constructors -}

mkVar :: VarInfo -> Index -> Node
mkVar i idx = NVar (Var i idx)

mkVar' :: Index -> Node
mkVar' = mkVar (VarInfo Nothing TyDynamic)

mkIdent :: IdentInfo -> Symbol -> Node
mkIdent i sym = NIdt (Ident i sym)

mkIdent' :: Symbol -> Node
mkIdent' = mkIdent (IdentInfo Nothing TyDynamic)

mkConstant :: ConstantValue -> Node
mkConstant cv = NCst (Constant () cv)

mkApps :: Fun -> [Node] -> Node
mkApps l r = NApp (Apps () l r)

mkBuiltinApp :: BuiltinOp -> [Node] -> Node
mkBuiltinApp op args = NBlt (BuiltinApp () op args)

mkConstr :: ConstrInfo -> Tag -> [Node] -> Node
mkConstr i tag args = NCtr (Constr i tag args)

mkConstr' :: Tag -> [Node] -> Node
mkConstr' = mkConstr (ConstrInfo Nothing TyDynamic)

mkLet :: LetInfo -> Node -> Node -> Node
mkLet i v b = NLet (Let i v b)

mkLet' :: Node -> Node -> Node
mkLet' = mkLet (LetInfo Nothing TyDynamic)

mkCase :: Node -> [CaseBranch] -> Maybe Node -> Node
mkCase v bs def = NCase (Case () v bs def)
