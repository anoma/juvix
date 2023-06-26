module Juvix.Compiler.Core.Transformation
  ( module Juvix.Compiler.Core.Transformation.Base,
    module Juvix.Compiler.Core.Transformation,
    module Juvix.Compiler.Core.Transformation.Eta,
    module Juvix.Compiler.Core.Transformation.LambdaLetRecLifting,
    module Juvix.Compiler.Core.Transformation.TopEtaExpand,
    module Juvix.Compiler.Core.Data.TransformationId,
  )
where

import Juvix.Compiler.Core.Data.TransformationId
import Juvix.Compiler.Core.Error
import Juvix.Compiler.Core.Options
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Transformation.Check.Exec
import Juvix.Compiler.Core.Transformation.Check.Geb
import Juvix.Compiler.Core.Transformation.Check.VampIR
import Juvix.Compiler.Core.Transformation.ComputeTypeInfo
import Juvix.Compiler.Core.Transformation.ConvertBuiltinTypes
import Juvix.Compiler.Core.Transformation.DisambiguateNames
import Juvix.Compiler.Core.Transformation.Eta
import Juvix.Compiler.Core.Transformation.FoldTypeSynonyms
import Juvix.Compiler.Core.Transformation.Identity
import Juvix.Compiler.Core.Transformation.IntToPrimInt
import Juvix.Compiler.Core.Transformation.LambdaLetRecLifting
import Juvix.Compiler.Core.Transformation.LetHoisting
import Juvix.Compiler.Core.Transformation.MatchToCase
import Juvix.Compiler.Core.Transformation.MoveApps
import Juvix.Compiler.Core.Transformation.NaiveMatchToCase qualified as Naive
import Juvix.Compiler.Core.Transformation.NatToPrimInt
import Juvix.Compiler.Core.Transformation.Normalize
import Juvix.Compiler.Core.Transformation.Optimize.CaseCallLifting
import Juvix.Compiler.Core.Transformation.Optimize.Inlining
import Juvix.Compiler.Core.Transformation.Optimize.LambdaFolding
import Juvix.Compiler.Core.Transformation.Optimize.LetFolding
import Juvix.Compiler.Core.Transformation.Optimize.Phase.Eval qualified as Phase.Eval
import Juvix.Compiler.Core.Transformation.Optimize.Phase.Exec qualified as Phase.Exec
import Juvix.Compiler.Core.Transformation.Optimize.Phase.Geb qualified as Phase.Geb
import Juvix.Compiler.Core.Transformation.Optimize.Phase.Main qualified as Phase.Main
import Juvix.Compiler.Core.Transformation.Optimize.Phase.VampIR qualified as Phase.VampIR
import Juvix.Compiler.Core.Transformation.Optimize.SimplifyIfs
import Juvix.Compiler.Core.Transformation.Optimize.SpecializeArgs
import Juvix.Compiler.Core.Transformation.RemoveTypeArgs
import Juvix.Compiler.Core.Transformation.TopEtaExpand
import Juvix.Compiler.Core.Transformation.UnrollRecursion

applyTransformations :: forall r. Members '[Error JuvixError, Reader CoreOptions] r => [TransformationId] -> InfoTable -> Sem r InfoTable
applyTransformations ts tbl = foldM (flip appTrans) tbl ts
  where
    appTrans :: TransformationId -> InfoTable -> Sem r InfoTable
    appTrans = \case
      LambdaLetRecLifting -> return . lambdaLetRecLifting
      LetRecLifting -> return . letRecLifting
      Identity -> return . identity
      TopEtaExpand -> return . topEtaExpand
      RemoveTypeArgs -> return . removeTypeArgs
      MoveApps -> return . moveApps
      NatToPrimInt -> return . natToPrimInt
      IntToPrimInt -> return . intToPrimInt
      ConvertBuiltinTypes -> return . convertBuiltinTypes
      ComputeTypeInfo -> return . computeTypeInfo
      UnrollRecursion -> unrollRecursion
      MatchToCase -> mapError (JuvixError @CoreError) . matchToCase
      NaiveMatchToCase -> return . Naive.matchToCase
      EtaExpandApps -> return . etaExpansionApps
      DisambiguateNames -> return . disambiguateNames
      CheckGeb -> mapError (JuvixError @CoreError) . checkGeb
      CheckExec -> mapError (JuvixError @CoreError) . checkExec
      CheckVampIR -> mapError (JuvixError @CoreError) . checkVampIR
      Normalize -> return . normalize
      LetFolding -> return . letFolding
      LambdaFolding -> return . lambdaFolding
      LetHoisting -> return . letHoisting
      Inlining -> inlining
      FoldTypeSynonyms -> return . foldTypeSynonyms
      CaseCallLifting -> return . caseCallLifting
      SimplifyIfs -> return . simplifyIfs
      SpecializeArgs -> return . specializeArgs
      OptPhaseEval -> Phase.Eval.optimize
      OptPhaseExec -> Phase.Exec.optimize
      OptPhaseGeb -> Phase.Geb.optimize
      OptPhaseVampIR -> Phase.VampIR.optimize
      OptPhaseMain -> Phase.Main.optimize
