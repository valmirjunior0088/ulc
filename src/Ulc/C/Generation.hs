module Ulc.C.Generation
  (Statement (..)
  ,Function (..)
  ,Item (..)
  ,generate
  )
  where

import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Writer (Writer, execWriter, tell)
import qualified Ulc.Common as Common

import Ulc.Common
  (Literal (..)
  ,Primitive (..)
  ,Variable (..)
  ,Term (..)
  ,Definition (..)
  ,Abstraction (..)
  )

data Statement =
  StEnter String |
  StLeave String |
  StInteger String Int |
  StIntegerSum String String String |
  StReal String Float |
  StRealSum String String String |
  StClosure String String [String] |
  StApply String String String |
  StCall String String |
  StReturn String |
  StNewLine
  deriving (Show)

data Function =
  Function String [Statement]
  deriving (Show)

data Item =
  Item Function [Function]

access :: Variable -> String
access variable =
  case variable of
    VrArgument -> "arg"
    VrEnvironment index -> "env[" ++ show index ++ "]"

enter :: Variable -> Statement
enter =
  StEnter . access

leave :: Variable -> Statement
leave =
  StLeave . access

type Generation =
  StateT Int (Writer [Statement])

runGeneration :: Generation () -> [Statement]
runGeneration action =
  execWriter (evalStateT action 0)

fresh :: Generation String
fresh = do
  value <- get
  put (succ value)
  return ("tmp_" ++ show value)

emit :: Term -> Generation String
emit term =
  case term of
    TrLiteral (LtInteger integer) -> do
      tmp <- fresh
      tell [StInteger tmp integer]
      return tmp
    TrLiteral (LtReal real) -> do
      tmp <- fresh
      tell [StReal tmp real]
      return tmp
    TrPrimitive (PrIntegerSum left right) -> do
      tmpLeft <- emit left
      tmpRight <- emit right
      tmp <- fresh
      tell [StIntegerSum tmp tmpLeft tmpRight]
      return tmp
    TrPrimitive (PrRealSum left right) -> do
      tmpLeft <- emit left
      tmpRight <- emit right
      tmp <- fresh
      tell [StRealSum tmp tmpLeft tmpRight]
      return tmp
    TrReference reference -> do
      tmp <- fresh
      tell [StCall tmp reference]
      return tmp
    TrVariable variable -> do
      tell [enter variable]
      return (access variable)
    TrClosure name variables -> do
      tmp <- fresh
      tell (map enter variables)
      tell [StClosure tmp name (map access variables)]
      return tmp
    TrApplication function argument -> do
      tmpFunction <- emit function
      tmpArgument <- emit argument
      tmp <- fresh
      tell [StApply tmp tmpFunction tmpArgument]
      return tmp

generateAbstraction :: Abstraction -> Function
generateAbstraction (Abstraction name size term) =
  Function name statements where
    variables = map VrEnvironment [0 .. pred size] ++ [VrArgument]
    statements = runGeneration $ do
      tmp <- emit term
      tell [StNewLine]
      tell (map leave variables)
      tell [StNewLine, StReturn tmp]

generateDefinition :: Definition -> Function
generateDefinition (Definition name term) =
  Function name statements where
    statements = runGeneration $ do
      tmp <- emit term
      tell [StNewLine, StReturn tmp]

generate :: Common.Item -> Item
generate (Common.Item definition abstractions) =
  Item (generateDefinition definition) (map generateAbstraction abstractions)
