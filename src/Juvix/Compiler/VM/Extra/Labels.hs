module Juvix.Compiler.VM.Extra.Labels where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.VM.Language
import Juvix.Data.PPOutput

data LabelError = ErrUndeclaredLabel Text | ErrDuplicateLabel Text

makeLenses ''LabelError

instance ToGenericError LabelError where
  genericError :: (Member (Reader GenericOptions) r) => LabelError -> Sem r GenericError
  genericError e = ask >>= generr
    where
      generr :: GenericOptions -> Sem r GenericError
      generr _ =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = ppOutput (pretty e),
              _genericErrorIntervals = [i]
            }
        where
          i = defaultLoc

          mockFile :: Path Abs File
          mockFile = $(mkAbsFile "/vm-run")

          defaultLoc :: Interval
          defaultLoc = singletonInterval (mkInitialLoc mockFile)

instance Pretty LabelError where
  pretty e =
    case e of
      ErrUndeclaredLabel lab -> "undeclared label: " <> pretty lab
      ErrDuplicateLabel lab -> "duplicate label: " <> pretty lab

resolveLabels ::
  Member (Error LabelError) r => [Instruction] -> Sem r [Instruction]
resolveLabels instrs0 = do
  (s :: HashMap Text Int, instrs) <- runState mempty $ computeAddrs 0 instrs0
  runReader s $ substAddrs 0 instrs
  where
    computeAddrs ::
      forall r.
      Members '[State (HashMap Text Int), Error LabelError] r =>
      Int ->
      [Instruction] ->
      Sem r [Instruction]
    computeAddrs _ [] = return []
    computeAddrs n (Label InstrLabel {..} : instrs) = do
      s :: HashMap Text Int <- get
      when (HashMap.member _instrLabelName s) $
        throw $
          ErrDuplicateLabel _instrLabelName
      modify' (HashMap.insert _instrLabelName n)
      computeAddrs n instrs
    computeAddrs n (instr : instrs) = do
      (instr :) <$> computeAddrs (n + 1) instrs

    substAddrs ::
      forall r.
      Members '[Reader (HashMap Text Int), Error LabelError] r =>
      Int ->
      [Instruction] ->
      Sem r [Instruction]
    substAddrs _ [] = return []
    substAddrs n (instr : instrs) = do
      instrs' <- substAddrs (n + 1) instrs
      instr' <-
        case instr of
          Binop x -> Binop <$> goBinop x
          JumpOnZero x -> JumpOnZero <$> goJumpOnZero x
          Halt -> return Halt
          Move x -> Move <$> goMove x
          Jump x -> Jump <$> goJump x
          Label {} -> impossible
      return $ instr' : instrs'
      where
        adjustValue :: Value -> Sem r Value
        adjustValue = \case
          RegRef r -> return $ RegRef r
          MemRef r -> return $ MemRef r
          Const num -> return $ Const num
          VarRef x -> return $ VarRef x
          LabelRef lab -> do
            maddr <- asks (HashMap.lookup lab)
            case maddr of
              Just addr -> return $ Const addr
              Nothing -> throw $ ErrUndeclaredLabel lab

        goBinop :: BinaryOp -> Sem r BinaryOp
        goBinop =
          overM binaryOpArg1 adjustValue
            >=> overM binaryOpArg2 adjustValue

        goMove :: InstrMove -> Sem r InstrMove
        goMove = overM instrMoveValue adjustValue

        goJump :: InstrJump -> Sem r InstrJump
        goJump = overM instrJumpDest adjustValue

        goJumpOnZero :: InstrJumpOnZero -> Sem r InstrJumpOnZero
        goJumpOnZero = overM instrJumpOnZeroDest adjustValue
