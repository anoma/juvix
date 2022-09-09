module Juvix.Compiler.Asm.Extra.Memory where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Asm.Data.Stack (Stack)
import Juvix.Compiler.Asm.Data.Stack qualified as Stack
import Juvix.Compiler.Asm.Error
import Juvix.Compiler.Asm.Extra.Type
import Juvix.Compiler.Asm.Language
import Juvix.Compiler.Asm.Language.Type
import Safe (atMay)

type Arguments = HashMap Offset Type

-- | A static representation of JuvixAsm memory providing type information for
-- memory locations.
data Memory = Memory
  { _memoryValueStack :: Stack Type,
    _memoryTempStack :: Stack Type,
    _memoryArgumentArea :: Arguments,
    _memoryArgsNum :: Int
  }

makeLenses ''Memory

mkMemory :: Arguments -> Memory
mkMemory args =
  Memory
    { _memoryValueStack = Stack.empty,
      _memoryTempStack = Stack.empty,
      _memoryArgumentArea = args,
      _memoryArgsNum = HashMap.size args
    }

pushValueStack :: Type -> Memory -> Memory
pushValueStack ty = over memoryValueStack (Stack.push ty)

popValueStack :: Int -> Memory -> Memory
popValueStack n = foldr (.) id (replicate n (over memoryValueStack Stack.pop))

pushTempStack :: Type -> Memory -> Memory
pushTempStack ty = over memoryTempStack (Stack.push ty)

popTempStack :: Int -> Memory -> Memory
popTempStack n = foldr (.) id (replicate n (over memoryTempStack Stack.pop))

-- | Read value stack at index `n` from the top.
topValueStack :: Int -> Memory -> Maybe Type
topValueStack n mem = Stack.nthFromTop n (mem ^. memoryValueStack)

topValueStack' :: Int -> Memory -> Type
topValueStack' n mem =
  fromMaybe (error "invalid value stack access") $
    topValueStack n mem

-- | Values from top of the value stack, in order from top to bottom.
topValuesFromValueStack :: Int -> Memory -> Maybe [Type]
topValuesFromValueStack n mem = Stack.topValues n (mem ^. memoryValueStack)

-- | Values from top of the value stack, in order from top to bottom.
topValuesFromValueStack' :: Int -> Memory -> [Type]
topValuesFromValueStack' n mem =
  fromMaybe (error "invalid value stack access") $
    Stack.topValues n (mem ^. memoryValueStack)

-- | Read temporary stack at index `n` from the bottom.
bottomTempStack :: Int -> Memory -> Maybe Type
bottomTempStack n mem = Stack.nthFromBottom n (mem ^. memoryTempStack)

bottomTempStack' :: Int -> Memory -> Type
bottomTempStack' n mem =
  fromMaybe (error "invalid temporary stack access") $
    bottomTempStack n mem

getArgumentType :: Offset -> Memory -> Maybe Type
getArgumentType off mem = HashMap.lookup off (mem ^. memoryArgumentArea)

getMemValueType :: MemValue -> Memory -> Maybe Type
getMemValueType val mem = case val of
  StackRef ->
    topValueStack 0 mem
  ArgRef off ->
    getArgumentType off mem
  TempRef off ->
    bottomTempStack off mem
  ConstrRef fld ->
    case getMemValueType (fld ^. fieldValue) mem of
      Just TyDynamic ->
        Just TyDynamic
      Just (TyInductive _) ->
        Just TyDynamic
      Just (TyConstr TypeConstr {..}) ->
        atMay _typeConstrFields (fld ^. fieldOffset)
      _ ->
        Nothing

-- | Check if the values on top of the value stack have the given types (the
-- last element of the list corresponds to the top of the stack)
checkValueStack :: Member (Error AsmError) r => Maybe Location -> [Type] -> Memory -> Sem r ()
checkValueStack loc tys mem = do
  unless (length (mem ^. memoryValueStack) >= length tys) $
    throw $
      AsmError
        loc
        ( fromString $
            "wrong value stack height: expected at least "
              ++ show (length tys)
              ++ ", but the height is "
              ++ show (length (mem ^. memoryValueStack))
        )
  mapM_
    ( \(ty, idx) -> do
        let ty' = fromJust $ topValueStack idx mem
        unless (isSubtype ty' ty) $
          throw $
            AsmError loc $
              fromString $
                "type mismatch on value stack cell " ++ show idx ++ " from top"
    )
    (zip (reverse tys) [0 ..])
