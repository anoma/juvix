module Juvix.Compiler.Nockma.Highlight.Doc (nockOpDoc, allRules) where

import Juvix.Compiler.Nockma.Highlight.Doc.Base
import Juvix.Compiler.Nockma.Highlight.Doc.Parser
import Juvix.Compiler.Nockma.Highlight.Doc.Pretty ()
import Juvix.Data.CodeAnn as CodeAnn
import Juvix.Prelude

allRules :: Doc CodeAnn
allRules =
  concatWith
    ( \a b ->
        a
          <> hardline
          <> hardline
          <> hardline
          <> hardline
          <> hardline
          <> hardline
          <> b
    )
    (map ppRule allElements)
  where
    ppRule :: NockOp -> Doc CodeAnn
    ppRule op =
      ppCodeAnn op
        <+> "("
          <> show op
          <> ")"
        <+> "evaluation rules:"
          <> hardline
          <> nockOpDoc op

nockOpDoc :: NockOp -> Doc CodeAnn
nockOpDoc n = ppCodeAnn $ case n of
  OpAddress ->
    [rules|
    ---
    s * [@ p] => index(s; p)
    |]
  OpQuote ->
    [rules|
    ---
    s * [quote t] => t
    |]
  OpApply ->
    [rules|
    s * t1 => t1' && s * t2 => t2'  && t1' * t2' => t'
    ---
    s * [apply [t1 t2]] => t'
    |]
  OpIsCell ->
    [rules|
    s * t => [t1' t2']
    ---
    s * [isCell t] => 0
    and
    s * t => a
    ---
    s * [isCell t] => 1
    |]
  OpInc ->
    [rules|
    s * t => n
    ---
    s * [suc t] => suc(t)
    |]
  OpEq ->
    [rules|
    ---
    s * [= [t t]] => 0
    and
    neq(t1; t2)
    ---
    s * [= [t1 t2]] => 1
    |]
  OpIf ->
    [rules|
    s * t0 => 0 && s * t1 => t1'
    ---
    s * [if [t0 [t1 t2]]] => t1'
    and
    s * t0 => 1 && s * t2 => t2'
    ---
    s * [if [t0 [t1 t2]]] => t2'
    |]
  OpSequence ->
    [rules|
    s * t1 => t1' && t1' * t2 => t'
    ---
    s * [seq [t1 t2]] => t'
    |]
  OpPush ->
    [rules|
    s * t1 => t1' && [t1' s] * t2 => t'
    ---
    s * [push [t1 t2]] => t'
    |]
  OpCall ->
    [rules|
    s * t => t' && t' * index(t'; p) => t''
    ---
    s * [call [p t]] => t''
    |]
  OpReplace ->
    [rules|
    s * t1 => t1' && s * t2 => t2'
    ---
    s * [replace [[p t1] t2]] => replace(t2';p;t1')
    |]
  OpHint ->
    [rules|
    s * t2 => t2' && s * t3 => t3'
    ---
    s * [hint [[t1 t2] t3]] => t3'
    |]
  OpScry ->
    [rules|
    s * t1 => t1' && s * t2 => t2'
    ---
    s * [scry [t1 t2]] => index(storage; t2')
    |]
