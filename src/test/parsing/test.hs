{-# HLINT ignore #-}
module Test where

import MiniJuvix.Parsing.ParserQQ

m :: Module 'Parsed 'ModuleTop
m = [mjuvixMod|
  module M;
  end;
  |]

