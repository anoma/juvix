module Juvix.Compiler.Internal.Translation.Extra
  ( module Juvix.Compiler.Internal.Translation,
    module Juvix.Compiler.Internal.Translation.Extra,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Translation
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context
import Juvix.Prelude

unfoldPolyApplication :: (Member (Reader TypesTable) r) => Application -> Sem r (Expression, [Expression])
unfoldPolyApplication a =
  let (f, args) = unfoldApplication a
   in case f of
        ExpressionLiteral {} -> return (f, toList args)
        ExpressionIden iden -> do
          args' <- filterCompileTimeArgsOrPatterns (getName iden) (toList args)
          return (f, args')
        ExpressionSimpleLambda {} -> return (f, toList args)
        ExpressionLambda {} -> return (f, toList args)
        _ -> impossible

filterCompileTimeArgsOrPatterns :: (Member (Reader TypesTable) r) => Name -> [a] -> Sem r [a]
filterCompileTimeArgsOrPatterns idenName lst = do
  tab <- ask
  let funParams = fst (unfoldFunType (ty tab))
      typedArgs =
        map fst $
          filter (not . isUniverse . snd) $
            zip lst (map (^. paramType) funParams)
  return $ typedArgs ++ drop (length funParams) lst
  where
    ty = HashMap.lookupDefault impossible (idenName ^. nameId)
    isUniverse :: Expression -> Bool
    isUniverse = \case
      (ExpressionUniverse {}) -> True
      _ -> False
