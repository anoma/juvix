module Juvix.Compiler.Core.Transformation.Trace
  ( trace,
  )
where

import Debug.Trace qualified as Trace
import Juvix.Compiler.Core.Pretty
import Juvix.Compiler.Core.Transformation.Base hiding (trace)
import Juvix.Compiler.Core.Transformation.DisambiguateNames

trace :: Module -> Module
trace = Trace.traceWith (fromText . ppTrace . (^. moduleInfoTable) . disambiguateNames)
