module Juvix.Compiler.Core.Translation.Base where

import Juvix.Compiler.Core.Language

freshName ::
  Member NameIdGen r =>
  NameKind ->
  Text ->
  Interval ->
  Sem r Name
freshName kind txt i = do
  nid <- freshNameId
  return $
    Name
      { _nameText = txt,
        _nameId = nid,
        _nameKind = kind,
        _namePretty = txt,
        _nameLoc = i
      }
