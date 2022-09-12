module Commands.Dev.Internal.Typecheck.Options where

import Juvix.Prelude
import Options.Applicative

newtype InternalTypeOptions = InternalTypeOptions
  { _microJuvixTypePrint :: Bool
  }
  deriving stock (Data)

makeLenses ''InternalTypeOptions

defaultInternalTypeOptions :: InternalTypeOptions
defaultInternalTypeOptions =
  InternalTypeOptions
    { _microJuvixTypePrint = False
    }

instance Semigroup InternalTypeOptions where
  o1 <> o2 =
    InternalTypeOptions
      { _microJuvixTypePrint = (o1 ^. microJuvixTypePrint) || (o2 ^. microJuvixTypePrint)
      }

instance Monoid InternalTypeOptions where
  mempty = defaultInternalTypeOptions
  mappend = (<>)

parseInternalType :: Parser InternalTypeOptions
parseInternalType = do
  _microJuvixTypePrint <-
    switch
      ( long "print-result"
          <> help "Print the type checked module if successful"
      )
  pure InternalTypeOptions {..}
