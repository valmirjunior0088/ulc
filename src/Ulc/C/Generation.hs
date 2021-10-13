module Ulc.C.Generation
  ( Instruction (..)
  , Function (..)
  , Item (..)
  , generate
  )
  where

import qualified Ulc.Common as Common
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Writer (Writer, execWriter, tell)

import Ulc.Common
  ( Literal (..)
  , Primitive (..)
  , Variable (..)
  , Term (..)
  , Definition (..)
  , Abstraction (..)
  , variablesFromSize
  )

data Instruction =
  InEnter String |
  InLeave String |
  InInteger String Int |
  InIntegerSum String String String |
  InReal String Float |
  InRealSum String String String |
  InClosure String String [String] |
  InApply String String String |
  InReference String String |
  InReturn String |
  InNewLine
  deriving (Show)

data Function =
  Function String [Instruction]
  deriving (Show)

data Item =
  Item Function [Function]

access :: Variable -> String
access variable =
  case variable of
    VrArgument -> "arg"
    VrEnvironment index -> "env[" ++ show index ++ "]"

enter :: Variable -> Instruction
enter =
  InEnter . access

leave :: Variable -> Instruction
leave =
  InLeave . access

type Generation =
  StateT Int (Writer [Instruction])

runGeneration :: Generation () -> [Instruction]
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
      tell [InInteger tmp integer]
      return tmp
    TrLiteral (LtReal real) -> do
      tmp <- fresh
      tell [InReal tmp real]
      return tmp
    TrPrimitive (PrIntegerSum left right) -> do
      tmpLeft <- emit left
      tmpRight <- emit right
      tmp <- fresh
      tell [InIntegerSum tmp tmpLeft tmpRight]
      return tmp
    TrPrimitive (PrRealSum left right) -> do
      tmpLeft <- emit left
      tmpRight <- emit right
      tmp <- fresh
      tell [InRealSum tmp tmpLeft tmpRight]
      return tmp
    TrReference reference -> do
      tmp <- fresh
      tell [InReference tmp reference]
      return tmp
    TrVariable variable -> do
      tell [enter variable]
      return (access variable)
    TrClosure name variables -> do
      tmp <- fresh
      tell (map enter variables)
      tell [InClosure tmp name (map access variables)]
      return tmp
    TrApplication function argument -> do
      tmpFunction <- emit function
      tmpArgument <- emit argument
      tmp <- fresh
      tell [InApply tmp tmpFunction tmpArgument]
      return tmp

generateAbstraction :: Abstraction -> Function
generateAbstraction (Abstraction name size term) =
  Function name instructions where
    instructions = runGeneration $ do
      tmp <- emit term
      tell [InNewLine]
      tell (map leave $ variablesFromSize size)
      tell [InNewLine, InReturn tmp]

generateDefinition :: Definition -> Function
generateDefinition (Definition name term) =
  Function name instructions where
    instructions = runGeneration $ do
      tmp <- emit term
      tell [InNewLine, InReturn tmp]

generate :: Common.Item -> Item
generate (Common.Item definition abstractions) =
  Item (generateDefinition definition) (map generateAbstraction abstractions)
