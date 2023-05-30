module Juvix.Data.Yaml
  ( module Data.Aeson.BetterErrors,
    module Data.Yaml,
    module Juvix.Data.Yaml,
  )
where

import Data.Aeson.BetterErrors hiding ((<|>))
import Data.Yaml (FromJSON (..))
import Juvix.Prelude.Base

type YamlError = Text

checkYamlKeys :: [Text] -> Parse Text ()
checkYamlKeys keys = do
  forEachInObject
    ( \k ->
        unless (k `elem` keys) $
          throwCustomError ("unknown key: " <> k)
    )
  return ()
