module Ulc.Common
  (Literal (..)
  ,Primitive (..)
  ,Variable (..)
  ,Term (..)
  ,Definition (..)
  ,Abstraction (..)
  ,Item (..)
  ,prepare
  )
  where

import Ulc.Common.Parsing (parse)
import Ulc.Common.Conversion (convert)

import Ulc.Common.Flattening
  (Literal (..)
  ,Primitive (..)
  ,Variable (..)
  ,Term (..)
  ,Definition (..)
  ,Abstraction (..)
  ,Item (..)
  ,flatten
  )

prepare :: String -> Either String [Item]
prepare source = do
  items <- parse source
  return (map (flatten . convert) items)
