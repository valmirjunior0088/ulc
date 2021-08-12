module Ulc.Conversion
  (Literal (..)
  ,Primitive (..)
  ,Variable (..)
  ,Term (..)
  ,convert
  )
  where

import Data.List (elemIndex)
import Control.Monad.State (State, get, put, runState, evalState)
import qualified Ulc.Core as Core
import Ulc.Core (Literal (..))

data Primitive =
  PrIntegerSum Term Term |
  PrRealSum Term Term
  deriving (Show)

data Variable =
  VrArgument |
  VrEnvironment Int
  deriving (Show)

data Term =
  TrLiteral Literal |
  TrPrimitive Primitive |
  TrReference String |
  TrVariable Variable |
  TrAbstraction [Variable] Term |
  TrApplication Term Term
  deriving (Show)

type Conversion =
  State [Core.Variable]

runConversion :: Conversion a -> (a, [Core.Variable])
runConversion action =
  runState action []

evalConversion :: Conversion a -> a
evalConversion action =
  evalState action []

rebind :: Core.Variable -> Conversion Variable
rebind variable =
  if variable == 0 then return VrArgument else do
    variables <- get
    case elemIndex (pred variable) variables of
      Nothing -> do
        put (variables ++ [pred variable])
        VrEnvironment <$> pure (length variables)
      Just index ->
        VrEnvironment <$> pure index

unwrap :: Core.Term -> Conversion Term
unwrap term =
  case term of
    Core.TrLiteral literal ->
      TrLiteral <$> pure literal
    Core.TrPrimitive (Core.PrIntegerSum left right) ->
      TrPrimitive <$> (PrIntegerSum <$> unwrap left <*> unwrap right)
    Core.TrPrimitive (Core.PrRealSum left right) ->
      TrPrimitive <$> (PrRealSum <$> unwrap left <*> unwrap right)
    Core.TrReference reference ->
      TrReference <$> pure reference
    Core.TrVariable variable ->
      TrVariable <$> rebind variable
    Core.TrAbstraction scope ->
      TrAbstraction <$> mapM rebind variables <*> pure convertedTerm where
        (convertedTerm, variables) = runConversion (unwrap scope)
    Core.TrApplication function argument ->
      TrApplication <$> unwrap function <*> unwrap argument

convert :: Core.Term -> Term
convert term =
  evalConversion (unwrap term)
