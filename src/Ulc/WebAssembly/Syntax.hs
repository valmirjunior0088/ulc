module Ulc.WebAssembly.Syntax
  ( Syntax
  , runSyntax
  , importFunc
  , importTable
  , importMemory
  , importGlobal
  , addFunc
  , addCode
  , getFunc
  , getFuncRef
  , getFuncRefsLength
  )
  where

import Data.List (elemIndex)
import Data.Int (Int32)
import Control.Monad.State (State, execState, get, put)

import Ulc.WebAssembly.Module
  ( TypeIdx
  , FuncIdx

  , RefType (..)
  , ValueType (..)
  , ResultType (..)
  , FuncType (..)
  , TypeSec (..)

  , Limits (..)
  , TableType (..)
  , MemoryType (..)
  , Mutability (..)
  , GlobalType (..)
  , ImportDesc (..)
  , Import (..)
  , ImportSec (..)

  , Func (..)
  , FuncSec (..)

  , Elem (..)
  , ElemSec (..)

  , buildLocals
  , Instr (..)
  , Expr (..)
  , Code (..)
  , CodeSec (..)

  , Module (..)
  , emptyModule

  , tableOffset
  )

type Syntax =
  State Module

runSyntax :: Syntax () -> Module
runSyntax action =
  execState action emptyModule

getType :: [ValueType] -> [ValueType] -> Syntax TypeIdx
getType inputs outputs = do
  let funcType = FuncType (ResultType inputs) (ResultType outputs)

  modl @ Module { mdTypes = TypeSec types } <- get

  case elemIndex funcType types of
    Nothing -> do
      put modl { mdTypes = TypeSec $ types ++ [funcType] }
      return (fromIntegral $ length types)
    Just typeIdx ->
      return (fromIntegral typeIdx)

-- IMPORTANT: Importing a function invalidates all function definition
-- indices. Import all functions before adding function definitions.
importFunc :: String -> String -> [ValueType] -> [ValueType] -> Syntax ()
importFunc namespace name inputs outputs = do
  typeIdx <- getType inputs outputs

  let
    imptDesc = IdFunc typeIdx
    impt = Import namespace name imptDesc

  modl @ Module { mdImports = ImportSec impts } <- get
  put modl { mdImports = ImportSec $ impts ++ [impt] }

importTable :: String -> String -> RefType -> Limits -> Syntax ()
importTable namespace name refType limits = do
  let
    tableType = TableType refType limits
    imptDesc = IdTable tableType
    impt = Import namespace name imptDesc

  modl @ Module { mdImports = ImportSec impts } <- get
  put modl { mdImports = ImportSec $ impts ++ [impt] }

importMemory :: String -> String -> Limits -> Syntax ()
importMemory namespace name limits = do
  let
    memoryType = MemoryType limits
    imptDesc = IdMemory memoryType
    impt = Import namespace name imptDesc

  modl @ Module { mdImports = ImportSec impts } <- get
  put modl { mdImports = ImportSec $ impts ++ [impt] }

importGlobal :: String -> String -> ValueType -> Mutability -> Syntax ()
importGlobal namespace name valueType mutability = do
  let
    globalType = GlobalType valueType mutability
    imptDesc = IdGlobal globalType
    impt = Import namespace name imptDesc

  modl @ Module { mdImports = ImportSec impts } <- get
  put modl { mdImports = ImportSec $ impts ++ [impt] }

addFunc :: String -> [(String, ValueType)] -> [ValueType] -> Syntax ()
addFunc name arguments outputs = do
  let
    inputs = map snd arguments
    inputNames = map fst arguments

  typeIdx <- getType inputs outputs

  modl @ Module { mdFuncs = FuncSec funcs } <- get
  put modl { mdFuncs = FuncSec $ funcs ++ [Func name inputNames typeIdx] }

addCode :: [(String, ValueType)] -> [Instr] -> Syntax ()
addCode types instructions = do
  let
    locals = buildLocals types
    expr = Expr instructions

  modl @ Module { mdCodes = CodeSec codes } <- get
  put modl { mdCodes = CodeSec $ codes ++ [Code locals expr] }

getFunc :: String -> Syntax FuncIdx
getFunc funcName = do
  Module { mdImports = ImportSec impts, mdFuncs = FuncSec funcs } <- get

  let
    imptFuncNames = [name | Import _ name (IdFunc _) <- impts]
    funcNames = [name | Func name _ _ <- funcs]

  case elemIndex funcName (imptFuncNames ++ funcNames) of
    Nothing -> error ("function doesn't exist: " ++ funcName)
    Just funcIdx -> return (fromIntegral funcIdx)

getFuncRef :: String -> Syntax Int32
getFuncRef name = do
  funcIdx <- getFunc name

  modl @ Module { mdElems = ElemSec elemSegments } <- get

  case elemSegments of
    [] -> do
      let
        offsetExpr = Expr [InI32Const $ fromIntegral $ tableOffset]
        elemSegment = Elem offsetExpr [funcIdx]
        funcRef = tableOffset + 0

      put modl { mdElems = ElemSec [elemSegment] }
      return (fromIntegral funcRef)
    
    [Elem offsetExpr funcIdxs] ->
      case elemIndex funcIdx funcIdxs of
        Nothing -> do
          let
            elemSegment = Elem offsetExpr (funcIdxs ++ [funcIdx])
            funcRef = tableOffset + length funcIdxs

          put modl { mdElems = ElemSec [elemSegment] }
          return (fromIntegral funcRef)

        Just funcRefIdx ->
          return (fromIntegral $ tableOffset + funcRefIdx)

    _ ->
      error "only a single funcref elem segment is supported"
  
getFuncRefsLength :: Syntax Int
getFuncRefsLength = do
  Module { mdElems = ElemSec elemSegments } <- get

  case elemSegments of
    [] -> return (tableOffset + 0)
    [Elem _ funcIdxs] -> return (tableOffset + length funcIdxs)
    _ -> error "only a single funcref elem segment is supported"
