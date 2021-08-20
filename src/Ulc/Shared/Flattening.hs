module Ulc.Shared.Flattening
  (Literal (..)
  ,Primitive (..)
  ,Variable (..)
  ,Term (..)
  ,Closure (..)
  ,Item (..)
  ,flatten
  )
  where

import Control.Monad.State (State, get, put, runState)
import Ulc.Shared.Conversion (Literal (..), Variable (..))
import qualified Ulc.Shared.Conversion as Conversion

data Primitive =
  PrIntegerSum Term Term |
  PrRealSum Term Term
  deriving (Show)

data Term =
  TrLiteral Literal |
  TrPrimitive Primitive |
  TrReference String |
  TrVariable Variable |
  TrClosure Int [Variable] |
  TrApplication Term Term
  deriving (Show)

data Closure =
  Closure Int Term
  deriving (Show)

data Item =
  Item String Term [Closure]

type Flattening =
  State [Closure]

runFlattening :: Flattening a -> (a, [Closure])
runFlattening action =
  runState action []

hoist :: Closure -> Flattening Int
hoist closure = do
  closures <- get
  put (closures ++ [closure])
  return (length closures)

unwrap :: Conversion.Term -> Flattening Term
unwrap term =
  case term of
    Conversion.TrLiteral literal ->
      TrLiteral <$> pure literal
    Conversion.TrPrimitive (Conversion.PrIntegerSum left right) ->
      TrPrimitive <$> (PrIntegerSum <$> unwrap left <*> unwrap right)
    Conversion.TrPrimitive (Conversion.PrRealSum left right) ->
      TrPrimitive <$> (PrRealSum <$> unwrap left <*> unwrap right)
    Conversion.TrReference reference ->
      TrReference <$> pure reference
    Conversion.TrVariable variable ->
      TrVariable <$> pure variable
    Conversion.TrAbstraction variables scope ->
      TrClosure <$> (unwrap scope >>= hoist . closure) <*> pure variables where
        closure = Closure (length variables)
    Conversion.TrApplication function argument ->
      TrApplication <$> unwrap function <*> unwrap argument

flatten :: Conversion.Item -> Item
flatten (Conversion.Item name term) =
  uncurry (Item name) (runFlattening $ unwrap term)
