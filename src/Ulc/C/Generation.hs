module Ulc.C.Generation
  (Statement (..)
  ,Function (..)
  ,Item (..)
  ,generate
  )
  where

import Control.Monad.Writer (Writer, tell, execWriter)
import Control.Monad.State (StateT, get, put, evalStateT)
import qualified Ulc.Shared as Shared

import Ulc.Shared
  (Literal (..)
  ,Primitive (..)
  ,Variable (..)
  ,Term (..)
  ,Closure (..)
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

mangle :: String -> Int -> String
mangle name index =
  name ++ "$" ++ show index

enumerate :: [a] -> [(Int, a)]
enumerate =
  zip [0 ..]

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

emit :: String -> Term -> Generation String
emit name term =
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
      tmpLeft <- emit name left
      tmpRight <- emit name right
      tmp <- fresh
      tell [StIntegerSum tmp tmpLeft tmpRight]
      return tmp
    TrPrimitive (PrRealSum left right) -> do
      tmpLeft <- emit name left
      tmpRight <- emit name right
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
    TrClosure index variables -> do
      tmp <- fresh
      tell (map enter variables)
      tell [StClosure tmp (mangle name index) (map access variables)]
      return tmp
    TrApplication function argument -> do
      tmpFunction <- emit name function
      tmpArgument <- emit name argument
      tmp <- fresh
      tell [StApply tmp tmpFunction tmpArgument]
      return tmp

generateAbstraction :: String -> (Int, Closure) -> Function
generateAbstraction name (index, Closure size term) =
  Function (mangle name index) statements where
    variables = map VrEnvironment [0 .. pred size] ++ [VrArgument]
    statements = runGeneration $ do
      tmp <- emit name term
      tell [StNewLine]
      tell (map leave variables)
      tell [StNewLine, StReturn tmp]

generateDefinition :: String -> Term -> Function
generateDefinition name term =
  Function name statements where
    statements = runGeneration $ do
      tmp <- emit name term
      tell [StNewLine, StReturn tmp]

generate :: Shared.Item -> Item
generate (Shared.Item name term closures) =
  Item definition abstractions where
    definition = generateDefinition name term
    abstractions = map (generateAbstraction name) (enumerate closures)
