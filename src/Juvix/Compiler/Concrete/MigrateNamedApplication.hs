module Juvix.Compiler.Concrete.MigrateNamedApplication where

import Juvix.Compiler.Concrete.Gen qualified as Gen
import Juvix.Compiler.Concrete.Keywords
import Juvix.Compiler.Concrete.Language.Base
import Juvix.Prelude

migrateNamedApplication :: forall s. (SingI s) => NamedApplication s -> NamedApplicationNew s
migrateNamedApplication old@NamedApplication {..} = run . runReader (getLoc old) $ do
  _namedApplicationNewAtKw <- Irrelevant <$> Gen.kw kwAt
  _namedApplicationNewExhaustive <- Gen.isExhaustive False
  return
    NamedApplicationNew
      { _namedApplicationNewName = _namedAppName,
        _namedApplicationNewArguments = migrateNamedApplicationArguments (toList _namedAppArgs),
        _namedApplicationNewExhaustive
      }
  where
    migrateNamedApplicationArguments :: [ArgumentBlock s] -> [NamedArgumentNew s]
    migrateNamedApplicationArguments = concatMap goBlock
      where
        goBlock :: ArgumentBlock s -> [NamedArgumentNew s]
        goBlock ArgumentBlock {..} = map goArg (toList _argBlockArgs)
          where
            goArg :: NamedArgumentAssign s -> NamedArgumentNew s
            goArg = NamedArgumentNewFunction . NamedArgumentFunctionDef . toFun

            toFun :: NamedArgumentAssign s -> FunctionDef s
            toFun NamedArgumentAssign {..} = Gen.simplestFunctionDef _namedArgName _namedArgValue
