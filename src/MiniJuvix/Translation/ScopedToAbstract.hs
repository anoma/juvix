module MiniJuvix.Translation.ScopedToAbstract where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Language
import MiniJuvix.Syntax.Concrete.Scoped.Name
import qualified MiniJuvix.Syntax.Abstract.Language as A


type Err = Text

translateModule :: Module 'Scoped 'ModuleTop -> Either Err A.Module
translateModule = undefined
