module Juvix.Compiler.Tree.Extra.Base where

import Juvix.Compiler.Tree.Language

mkBinop :: BinaryOpcode -> Node -> Node -> Node
mkBinop op arg1 arg2 = Binop (NodeBinop mempty op arg1 arg2)

mkUnop :: UnaryOpcode -> Node -> Node
mkUnop op arg = Unop (NodeUnop mempty op arg)

mkConst :: Constant -> Node
mkConst c = Constant (NodeConstant mempty c)

mkMemRef :: MemRef -> Node
mkMemRef r = MemRef (NodeMemRef mempty r)

getNodeInfo :: Node -> NodeInfo
getNodeInfo = \case
  Binop NodeBinop {..} -> _nodeBinopInfo
  Unop NodeUnop {..} -> _nodeUnopInfo
  Cairo NodeCairo {..} -> _nodeCairoInfo
  Constant NodeConstant {..} -> _nodeConstantInfo
  MemRef NodeMemRef {..} -> _nodeMemRefInfo
  AllocConstr NodeAllocConstr {..} -> _nodeAllocConstrInfo
  AllocClosure NodeAllocClosure {..} -> _nodeAllocClosureInfo
  ExtendClosure NodeExtendClosure {..} -> _nodeExtendClosureInfo
  Call NodeCall {..} -> _nodeCallInfo
  CallClosures NodeCallClosures {..} -> _nodeCallClosuresInfo
  Branch NodeBranch {..} -> _nodeBranchInfo
  Case NodeCase {..} -> _nodeCaseInfo
  Save NodeSave {..} -> _nodeSaveInfo

getNodeLocation :: Node -> Maybe Location
getNodeLocation = (^. nodeInfoLocation) . getNodeInfo

{------------------------------------------------------------------------}
{- generic Node destruction -}

data NodeChild = NodeChild
  { -- | immediate child of some node
    _childNode :: Node,
    -- | `Just i` if the child introduces a temporary variable
    _childTempVar :: Maybe TempVar
  }

makeLenses ''NodeChild

-- | `NodeDetails` is a convenience datatype which provides the most commonly needed
-- information about a node in a generic fashion.
data NodeDetails = NodeDetails
  { -- | 'nodeChildren' are the children, in a fixed order, i.e., the immediate
    -- recursive subnodes
    _nodeChildren :: [NodeChild],
    -- | 'nodeReassemble' reassembles the node from the children (which should
    -- be in the same fixed order as in 'nodeChildren').
    _nodeReassemble :: [Node] -> Node
  }

makeLenses ''NodeDetails

{-# INLINE noTempVar #-}
noTempVar :: Node -> NodeChild
noTempVar n =
  NodeChild
    { _childNode = n,
      _childTempVar = Nothing
    }

{-# INLINE oneTempVar #-}
oneTempVar :: TempVar -> Node -> NodeChild
oneTempVar i n =
  NodeChild
    { _childNode = n,
      _childTempVar = Just i
    }

type Reassemble = [Node] -> Node

{-# INLINE noChildren #-}
noChildren :: Node -> Reassemble
noChildren n ch = case ch of
  [] -> n
  _ -> impossible

{-# INLINE oneChild #-}
oneChild :: (Node -> Node) -> Reassemble
oneChild f ch = case ch of
  [c] -> f c
  _ -> impossible

{-# INLINE twoChildren #-}
twoChildren :: (Node -> Node -> Node) -> Reassemble
twoChildren f ch = case ch of
  [l, r] -> f l r
  _ -> impossible

{-# INLINE threeChildren #-}
threeChildren :: (Node -> Node -> Node -> Node) -> Reassemble
threeChildren f ch = case ch of
  [a, b, c] -> f a b c
  _ -> impossible

{-# INLINE manyChildren #-}
manyChildren :: ([Node] -> Node) -> Reassemble
manyChildren f = f

{-# INLINE someChildren #-}
someChildren :: (NonEmpty Node -> Node) -> Reassemble
someChildren f = f . nonEmpty'

{-# INLINE twoManyChildren #-}
twoManyChildren :: (Node -> Node -> [Node] -> Node) -> Reassemble
twoManyChildren f = \case
  (x : y : xs) -> f x y xs
  _ -> impossible

-- | Destruct a node into NodeDetails. This is an internal function used to
-- implement more high-level accessors and recursors.
destruct :: Node -> NodeDetails
destruct = \case
  Binop NodeBinop {..} ->
    NodeDetails
      { _nodeChildren = [noTempVar _nodeBinopArg1, noTempVar _nodeBinopArg2],
        _nodeReassemble = twoChildren $ \arg1 arg2 ->
          Binop
            NodeBinop
              { _nodeBinopArg1 = arg1,
                _nodeBinopArg2 = arg2,
                _nodeBinopOpcode,
                _nodeBinopInfo
              }
      }
  Unop NodeUnop {..} ->
    NodeDetails
      { _nodeChildren = [noTempVar _nodeUnopArg],
        _nodeReassemble = oneChild $ \arg ->
          Unop
            NodeUnop
              { _nodeUnopArg = arg,
                _nodeUnopOpcode,
                _nodeUnopInfo
              }
      }
  Cairo NodeCairo {..} ->
    NodeDetails
      { _nodeChildren = map noTempVar _nodeCairoArgs,
        _nodeReassemble = manyChildren $ \args ->
          Cairo
            NodeCairo
              { _nodeCairoArgs = args,
                _nodeCairoOpcode,
                _nodeCairoInfo
              }
      }
  Constant c ->
    NodeDetails
      { _nodeChildren = [],
        _nodeReassemble = noChildren (Constant c)
      }
  MemRef r ->
    NodeDetails
      { _nodeChildren = [],
        _nodeReassemble = noChildren (MemRef r)
      }
  AllocConstr NodeAllocConstr {..} ->
    NodeDetails
      { _nodeChildren = map noTempVar _nodeAllocConstrArgs,
        _nodeReassemble = manyChildren $ \args ->
          AllocConstr
            NodeAllocConstr
              { _nodeAllocConstrArgs = args,
                _nodeAllocConstrTag,
                _nodeAllocConstrInfo
              }
      }
  AllocClosure NodeAllocClosure {..} ->
    NodeDetails
      { _nodeChildren = map noTempVar _nodeAllocClosureArgs,
        _nodeReassemble = manyChildren $ \args ->
          AllocClosure
            NodeAllocClosure
              { _nodeAllocClosureArgs = args,
                _nodeAllocClosureFunSymbol,
                _nodeAllocClosureInfo
              }
      }
  ExtendClosure NodeExtendClosure {..} ->
    NodeDetails
      { _nodeChildren = map noTempVar (_nodeExtendClosureFun : toList _nodeExtendClosureArgs),
        _nodeReassemble = someChildren $ \(arg :| args) ->
          ExtendClosure
            NodeExtendClosure
              { _nodeExtendClosureArgs = nonEmpty' args,
                _nodeExtendClosureFun = arg,
                _nodeExtendClosureInfo
              }
      }
  Call NodeCall {..} -> case _nodeCallType of
    CallFun sym ->
      NodeDetails
        { _nodeChildren = map noTempVar _nodeCallArgs,
          _nodeReassemble = manyChildren $ \args ->
            Call
              NodeCall
                { _nodeCallArgs = args,
                  _nodeCallType = CallFun sym,
                  _nodeCallInfo
                }
        }
    CallClosure cl ->
      NodeDetails
        { _nodeChildren = map noTempVar (cl : _nodeCallArgs),
          _nodeReassemble = someChildren $ \(arg :| args) ->
            Call
              NodeCall
                { _nodeCallArgs = args,
                  _nodeCallType = CallClosure arg,
                  _nodeCallInfo
                }
        }
  CallClosures NodeCallClosures {..} ->
    NodeDetails
      { _nodeChildren = map noTempVar (_nodeCallClosuresFun : toList _nodeCallClosuresArgs),
        _nodeReassemble = someChildren $ \(arg :| args) ->
          CallClosures
            NodeCallClosures
              { _nodeCallClosuresArgs = nonEmpty' args,
                _nodeCallClosuresFun = arg,
                _nodeCallClosuresInfo
              }
      }
  Branch NodeBranch {..} ->
    NodeDetails
      { _nodeChildren = [noTempVar _nodeBranchArg, noTempVar _nodeBranchTrue, noTempVar _nodeBranchFalse],
        _nodeReassemble = threeChildren $ \arg br1 br2 ->
          Branch
            NodeBranch
              { _nodeBranchArg = arg,
                _nodeBranchTrue = br1,
                _nodeBranchFalse = br2,
                _nodeBranchInfo
              }
      }
  Case NodeCase {..} ->
    case _nodeCaseDefault of
      Nothing ->
        NodeDetails
          { _nodeChildren = noTempVar _nodeCaseArg : branchChildren,
            _nodeReassemble = someChildren $ \(v' :| bodies') ->
              Case
                NodeCase
                  { _nodeCaseArg = v',
                    _nodeCaseBranches = mkBranches _nodeCaseBranches bodies',
                    _nodeCaseDefault = Nothing,
                    _nodeCaseInductive,
                    _nodeCaseInfo
                  }
          }
      Just def ->
        NodeDetails
          { _nodeChildren = noTempVar _nodeCaseArg : noTempVar def : branchChildren,
            _nodeReassemble = twoManyChildren $ \v' def' bodies' ->
              Case
                NodeCase
                  { _nodeCaseArg = v',
                    _nodeCaseBranches = mkBranches _nodeCaseBranches bodies',
                    _nodeCaseDefault = Just def',
                    _nodeCaseInductive,
                    _nodeCaseInfo
                  }
          }
    where
      branchChildren = map mkBranchChild _nodeCaseBranches

      mkBranchChild :: CaseBranch -> NodeChild
      mkBranchChild CaseBranch {..} =
        (if _caseBranchSave then oneTempVar (TempVar Nothing Nothing) else noTempVar) _caseBranchBody

      mkBranches :: [CaseBranch] -> [Node] -> [CaseBranch]
      mkBranches = zipWithExact (flip (set caseBranchBody))
  Save NodeSave {..} ->
    NodeDetails
      { _nodeChildren = [noTempVar _nodeSaveArg, oneTempVar _nodeSaveTempVar _nodeSaveBody],
        _nodeReassemble = twoChildren $ \arg body ->
          Save
            NodeSave
              { _nodeSaveArg = arg,
                _nodeSaveBody = body,
                _nodeSaveTempVar,
                _nodeSaveInfo
              }
      }

reassembleDetails :: NodeDetails -> [Node] -> Node
reassembleDetails d ns = (d ^. nodeReassemble) ns

reassemble :: Node -> [Node] -> Node
reassemble = reassembleDetails . destruct

children :: Node -> [NodeChild]
children = (^. nodeChildren) . destruct

childrenNodes :: Node -> [Node]
childrenNodes = map (^. childNode) . children
