module Ulc.Flattening
  (Literal (..)
  ,Primitive (..)
  ,Variable (..)
  ,Term (..)
  ,Closure (..)
  ,flatten
  )
  where

import Control.Monad.State (State, get, put, runState)
import qualified Ulc.Conversion as Conversion
import Ulc.Conversion (Literal (..), Variable (..))

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

flatten :: Conversion.Term -> (Term, [Closure])
flatten term =
  runFlattening (unwrap term)
