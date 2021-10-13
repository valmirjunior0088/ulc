module Ulc.Common.Conversion
  ( Literal (..)
  , Primitive (..)
  , Variable (..)
  , Term (..)
  , Item (..)
  , convert
  )
  where

import Ulc.Common.Core (Literal (..))
import qualified Ulc.Common.Core as Core
import Data.List (elemIndex)
import Control.Monad.State (State, runState, evalState, get, put)

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

data Item =
  Item String Term

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
        return (VrEnvironment $ length variables)
      Just index ->
        return (VrEnvironment index)

unwrap :: Core.Term -> Conversion Term
unwrap term =
  case term of
    Core.TrLiteral literal ->
      return (TrLiteral literal)
    Core.TrPrimitive (Core.PrIntegerSum left right) -> do
      left' <- unwrap left
      right' <- unwrap right
      return (TrPrimitive $ PrIntegerSum left' right')
    Core.TrPrimitive (Core.PrRealSum left right) -> do
      left' <- unwrap left
      right' <- unwrap right
      return (TrPrimitive $ PrRealSum left' right')
    Core.TrReference reference ->
      return (TrReference reference)
    Core.TrVariable variable -> do
      variable' <- rebind variable
      return (TrVariable variable')
    Core.TrAbstraction scope -> do
      let (term', variables) = runConversion (unwrap scope)
      variables' <- mapM rebind variables
      return (TrAbstraction variables' term')
    Core.TrApplication function argument -> do
      function' <- unwrap function
      argument' <- unwrap argument
      return (TrApplication function' argument')

convert :: Core.Item -> Item
convert (Core.Item name term) =
  Item name (evalConversion $ unwrap term)
