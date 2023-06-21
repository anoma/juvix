module Juvix.Compiler.Abstract.Data.InfoTableBuilder
  ( module Juvix.Compiler.Abstract.Data.InfoTableBuilder,
    module Juvix.Compiler.Abstract.Data.InfoTable,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Abstract.Data.InfoTable
import Juvix.Compiler.Abstract.Language
import Juvix.Prelude

data InfoTableBuilder m a where
  RegisterAxiom :: AxiomDef -> InfoTableBuilder m ()
  RegisterConstructor :: InductiveInfo -> InductiveConstructorDef -> InfoTableBuilder m ()
  RegisterInductive :: InductiveDef -> InfoTableBuilder m InductiveInfo
  RegisterFunction :: FunctionDef -> InfoTableBuilder m ()

makeSem ''InfoTableBuilder

registerFunction' ::
  (Member InfoTableBuilder r) =>
  FunctionDef ->
  Sem r FunctionDef
registerFunction' ts = registerFunction ts $> ts

registerAxiom' ::
  (Member InfoTableBuilder r) =>
  AxiomDef ->
  Sem r AxiomDef
registerAxiom' a = registerAxiom a $> a

toState :: Sem (InfoTableBuilder ': r) a -> Sem (State InfoTable ': r) a
toState = reinterpret $ \case
  RegisterAxiom d ->
    let ref = AxiomRef (d ^. axiomName)
        info =
          AxiomInfo
            { _axiomInfoType = d ^. axiomType
            }
     in modify (over infoAxioms (HashMap.insert ref info))
  RegisterConstructor _constructorInfoInductive def ->
    let ref = def ^. constructorName
        info =
          ConstructorInfo
            { _constructorInfoType = def ^. constructorType,
              ..
            }
     in modify (over infoConstructors (HashMap.insert ref info))
  RegisterInductive ity ->
    let ref = InductiveRef (ity ^. inductiveName)
        info =
          InductiveInfo
            { _inductiveInfoDef = ity
            }
     in modify (over infoInductives (HashMap.insert ref info)) $> info
  RegisterFunction _functionInfoDef ->
    let ref = FunctionRef (_functionInfoDef ^. funDefName)
        info =
          FunctionInfo
            { ..
            }
     in modify (over infoFunctions (HashMap.insert ref info))

runInfoTableBuilder :: Sem (InfoTableBuilder ': r) a -> Sem r (InfoTable, a)
runInfoTableBuilder = runState emptyInfoTable . toState

runInfoTableBuilder' :: InfoTable -> Sem (InfoTableBuilder ': r) a -> Sem r (InfoTable, a)
runInfoTableBuilder' t = runState t . toState

ignoreInfoTableBuilder :: Sem (InfoTableBuilder ': r) a -> Sem r a
ignoreInfoTableBuilder = fmap snd . runInfoTableBuilder
