module Juvix.Syntax.Abstract.Quasiquoter where

--   ( module Juvix.Syntax.Abstract.Quasiquoter,
--     module Juvix.Syntax.Abstract.Language,
--   )
-- where

-- import Data.HashMap.Strict qualified as HashMap
-- import Language.Haskell.TH (Exp, Q)
-- import Language.Haskell.TH.Quote (QuasiQuoter (..))
-- import Language.Haskell.TH.Syntax qualified as TH
-- import Juvix.Internal.NameIdGen (runNameIdGen)
-- import Juvix.Pipeline
-- import Juvix.Prelude
-- import Juvix.Syntax.Abstract.AbstractResult
-- import Juvix.Syntax.Abstract.Language

-- juvixQ :: Lift a => (Module -> a) -> QuasiQuoter
-- juvixQ proj =
--   QuasiQuoter
--     { quotePat = impossible,
--       quoteType = impossible,
--       quoteDec = impossible,
--       quoteExp = juvixModule proj "Tmp" . wrapTmpModule
--     }
--     where
--     wrapTmpModule :: String -> String
--     wrapTmpModule body = "module Tmp;" <> body <> "end;"

-- functionQ :: QuasiQuoter
-- functionQ = juvixQ (getInd . getStatement)
--   where
--     getInd :: Statement -> FunctionDef
--     getInd = \case
--       StatementFunction d -> d
--       _ -> error "function definition not found"

-- inductiveQ :: QuasiQuoter
-- inductiveQ = juvixQ (getInd . getStatement)
--   where
--     getInd :: Statement -> InductiveDef
--     getInd = \case
--       StatementInductive d -> d
--       _ -> error "inductive definition not found"

-- foreignQ :: QuasiQuoter
-- foreignQ = juvixQ (getForeign . getStatement)
--   where
--     getForeign :: Statement -> ForeignBlock
--     getForeign = \case
--       StatementForeign d -> d
--       _ -> error "foreign block not found"

-- getStatement :: Module -> Statement
-- getStatement = (^?! moduleBody . moduleStatements . _nonEmpty . _Just . _head)

-- readAbsractModule :: String -> Text -> Either JuvixError Module
-- readAbsractModule name body =
--   fmap (head . (^. resultModules))
--     . run
--     . runError
--     . runNameIdGen
--     . runFilesPure fs
--     $ upToAbstract entry
--   where
--     fs :: HashMap FilePath Text
--     fs = HashMap.singleton fakePath body
--     entry :: EntryPoint
--     entry =
--       EntryPoint
--         { _entryPointRoot = ".",
--           _entryPointNoTermination = False,
--           _entryPointModulePaths = fakePath :| []
--         }
--     fakePath :: FilePath
--     fakePath = name <> ".juvix"

-- juvixModule :: Lift a => (Module -> a) -> String -> String -> Q Exp
-- juvixModule proj modName str = case readAbsractModule modName (pack str) of
--   Left e -> fail ("Error: " <> unpack (renderText e))
--   Right r -> TH.lift (proj r)
