-- | Adapted from https://github.com/heliaxdev/juvix/
module MiniJuvix.Parsing.Parser where

import MiniJuvix.Parsing.Language
import MiniJuvix.Utils.Prelude
import qualified Text.Megaparsec as P
import MiniJuvix.Parsing.Lexer


moduleDef' ∷ MonadParsec e Text m ⇒ ReaderT (m PExpr) m PModuleDef
moduleDef' = do
  _typeDefs <- sepEndBy dataDef semi
  _funDefs <- catMaybes <$> sepEndBy anyDef semi
  let _extra = ()
  return $ ModuleDef{..}

moduleDef ∷ MonadParsec e Text m ⇒ ReaderT (m PExpr) m PModuleDef
moduleDef = space >> moduleDef' <* M.eof
