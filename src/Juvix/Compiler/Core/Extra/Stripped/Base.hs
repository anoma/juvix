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

mkConstr :: Tag -> [Node] -> Node
mkConstr tag args = NCtr (Constr () tag args)

mkLet :: Node -> Node -> Node
mkLet v b = NLet (Let () v b)

mkCase :: Node -> [CaseBranch] -> Maybe Node -> Node
mkCase v bs def = NCase (Case () v bs def)
