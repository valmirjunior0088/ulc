module Ulc.Common.Flattening
  (Literal (..)
  ,Primitive (..)
  ,Variable (..)
  ,Term (..)
  ,Definition (..)
  ,Abstraction (..)
  ,Item (..)
  ,flatten
  )
  where

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, get, put, evalStateT)
import Control.Monad.Writer (Writer, tell, runWriter)
import Ulc.Common.Conversion (Literal (..), Variable (..))
import qualified Ulc.Common.Conversion as Conversion

data Primitive =
  PrIntegerSum Term Term |
  PrRealSum Term Term
  deriving (Show)

data Term =
  TrLiteral Literal |
  TrPrimitive Primitive |
  TrReference String |
  TrVariable Variable |
  TrClosure String [Variable] |
  TrApplication Term Term
  deriving (Show)

data Definition =
  Definition String Term

data Abstraction =
  Abstraction String Int Term
  deriving (Show)

data Item =
  Item Definition [Abstraction]

type Flattening =
  ReaderT String (StateT Int (Writer [Abstraction]))

runFlattening :: Flattening a -> String -> (a, [Abstraction])
runFlattening action name =
  runWriter (evalStateT (runReaderT action name) 0)

fresh :: Flattening String
fresh = do
  name <- ask
  source <- get
  put (succ source)
  return (name ++ "$" ++ show source)

unwrap :: Conversion.Term -> Flattening Term
unwrap term =
  case term of
    Conversion.TrLiteral literal ->
      return (TrLiteral literal)
    Conversion.TrPrimitive (Conversion.PrIntegerSum left right) -> do
      left' <- unwrap left
      right' <- unwrap right
      return (TrPrimitive $ PrIntegerSum left' right')
    Conversion.TrPrimitive (Conversion.PrRealSum left right) -> do
      left' <- unwrap left
      right' <- unwrap right
      return (TrPrimitive $ PrRealSum left' right')
    Conversion.TrReference reference ->
      return (TrReference reference)
    Conversion.TrVariable variable -> 
      return (TrVariable variable)
    Conversion.TrAbstraction variables scope -> do
      body <- unwrap scope
      name <- fresh
      tell [Abstraction name (length variables) body]
      return (TrClosure name variables)
    Conversion.TrApplication function argument -> do
      function' <- unwrap function
      argument' <- unwrap argument
      return (TrApplication function' argument')

flatten :: Conversion.Item -> Item
flatten (Conversion.Item name term) =
  Item (Definition name definition) abstractions where
    (definition, abstractions) = runFlattening (unwrap term) name
