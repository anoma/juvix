module Juvix.Compiler.Internal.Builtins
  ( module Juvix.Compiler.Internal.Builtins,
    module Juvix.Compiler.Builtins.Effect,
  )
where

import Juvix.Compiler.Builtins.Effect hiding (getBuiltinSymbol, registerBuiltin)
import Juvix.Compiler.Builtins.Effect qualified as B
import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Pretty
import Juvix.Prelude

data FunInfo = FunInfo
  { _funInfoDef :: FunctionDef,
    _funInfoBuiltin :: BuiltinFunction,
    _funInfoSignature :: Expression,
    _funInfoClauses :: [(Expression, Expression)],
    _funInfoFreeVars :: [VarName],
    _funInfoFreeTypeVars :: [VarName]
  }

makeLenses ''FunInfo

getBuiltinName :: (IsBuiltin a, Members '[Builtins] r) => Interval -> a -> Sem r Name
getBuiltinName loc p = fromConcreteSymbol <$> (B.getBuiltinSymbol loc p)

checkBuiltinFunctionInfo ::
  forall r.
  (Members '[Error BuiltinsError] r) =>
  FunInfo ->
  Sem r ()
checkBuiltinFunctionInfo fi = do
  let op = fi ^. funInfoDef . funDefName
      ty = fi ^. funInfoDef . funDefType
      sig = fi ^. funInfoSignature
      err :: forall a. AnsiText -> Sem r a
      err msg =
        throw $
          ErrBuiltinsErrorMessage
            BuiltinsErrorMessage
              { _builtinsErrorMessageLoc = getLoc (fi ^. funInfoDef),
                _builtinsErrorMessage = msg
              }
  unless ((sig ==% ty) (hashSet (fi ^. funInfoFreeTypeVars))) (err "builtin has the wrong type signature")
  let freeVars = hashSet (fi ^. funInfoFreeVars)
      a =% b = (a ==% b) freeVars
      clauses :: [(Expression, Expression)]
      clauses =
        [ (clauseLhsAsExpression op (toList pats), body)
          | Just cls <- [unfoldLambdaClauses (fi ^. funInfoDef . funDefBody)],
            (pats, body) <- toList cls
        ]
  case zipExactMay (fi ^. funInfoClauses) clauses of
    Nothing -> err "builtin has the wrong number of clauses"
    Just z -> forM_ z $ \((exLhs, exBody), (lhs, body)) -> do
      unless
        (exLhs =% lhs)
        ( err
            ( "clause lhs does not match for "
                <> ppOutDefault op
                <> "\nExpected: "
                <> ppOutDefault exLhs
                <> "\nActual: "
                <> ppOutDefault lhs
            )
        )
      unless (exBody =% body) (error $ "clause body does not match " <> ppTrace exBody <> " | " <> ppTrace body)
