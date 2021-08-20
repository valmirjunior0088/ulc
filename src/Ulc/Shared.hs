module Ulc.Shared
  (Literal (..)
  ,Primitive (..)
  ,Variable (..)
  ,Term (..)
  ,Closure (..)
  ,Item (..)
  ,prepare
  )
  where

import Ulc.Shared.Parsing (parse)
import Ulc.Shared.Conversion (convert)

import Ulc.Shared.Flattening
  (Literal (..)
  ,Primitive (..)
  ,Variable (..)
  ,Term (..)
  ,Closure (..)
  ,Item (..)
  ,flatten
  )

prepare :: String -> Either String [Item]
prepare source = do
  items <- parse source
  return (map (flatten . convert) items)
