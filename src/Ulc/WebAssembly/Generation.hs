{-# LANGUAGE NamedFieldPuns #-}

module Ulc.WebAssembly.Generation
  ( TypeIdx
  , FuncIdx
  , ElemIdx
  , ValueType (..)
  , FuncType (..)
  , Func (..)
  , Instruction (..)
  , Code (..)
  , Module (..)
  , tableOffset
  , pointerSize
  , generate
  )
  where

import Data.List (elemIndex)
import Control.Monad.State (State, execState, get, put)
import Data.Word (Word32)
import Data.Int (Int32)

import Ulc.Common
  ( Literal (..)
  , Primitive (..)
  , Variable (..)
  , Term (..)
  , Definition (..)
  , Abstraction (..)
  , Item (..)
  , variablesFromSize
  )

type TypeIdx = Word32
type FuncIdx = Word32
type ElemIdx = Word32
type LocalIdx = Word32

type Offset = Word32
type Alignment = Word32

data ValueType =
  I32 |
  F32
  deriving (Eq, Show)

data FuncType =
  FuncType [ValueType] [ValueType]
  deriving (Eq, Show)

data Func =
  Func String TypeIdx
  deriving (Show)

data Instruction =
  I32Const Int32 |
  I32Elem ElemIdx |
  I32Load Offset Alignment |
  F32Const Float |
  LocalGet LocalIdx |
  LocalSet LocalIdx |
  Call FuncIdx
  deriving (Show)

data Code =
  Code [ValueType] [Instruction]
  deriving (Show)

data Module =
  Module
    { mdTypes :: [FuncType]
    , mdImports :: [Func]
    , mdFuncs :: [Func]
    , mdElems :: [FuncIdx]
    , mdCodes :: [Code]
    }
  deriving (Show)

mdEmpty :: Module
mdEmpty =
  Module
    { mdTypes = []
    , mdImports = []
    , mdFuncs = []
    , mdElems = []
    , mdCodes = []
    }

-- Elem 0 is reserved for the null function pointer
tableOffset :: Int
tableOffset = 1

-- Wasm32 has a pointer size of 4 bytes
pointerSize :: Int
pointerSize = 4

type Generation =
  State Module

runGeneration :: Generation () -> Module
runGeneration action =
  execState action mdEmpty

getType :: [ValueType] -> [ValueType] -> Generation TypeIdx
getType inputs outputs = do
  let funcType = FuncType inputs outputs

  modl @ Module { mdTypes } <- get
  case elemIndex funcType mdTypes of
    Nothing -> do
      put modl { mdTypes = mdTypes ++ [funcType] }
      return (fromIntegral $ length mdTypes)
    Just typeIdx ->
      return (fromIntegral typeIdx)

putImport :: String -> [ValueType] -> [ValueType] -> Generation ()
putImport name inputs outputs = do
  typeIdx <- getType inputs outputs

  modl @ Module { mdImports } <- get
  put modl { mdImports = mdImports ++ [Func name typeIdx] }

putFunc :: String -> [ValueType] -> [ValueType] -> Generation ()
putFunc name inputs outputs = do
  typeIdx <- getType inputs outputs

  modl @ Module { mdFuncs } <- get
  put modl { mdFuncs = mdFuncs ++ [Func name typeIdx] }

getFunc :: String -> Generation FuncIdx
getFunc funcName = do
  let fnName (Func name _) = name

  Module { mdImports, mdFuncs } <- get
  case elemIndex funcName (map fnName mdImports) of
    Just funcIdx -> return (fromIntegral funcIdx)
    Nothing -> case elemIndex funcName (map fnName mdFuncs) of
      Just funcIdx -> return (fromIntegral $ length mdImports + funcIdx)
      Nothing -> error "unknown function name"

getElem :: String -> Generation ElemIdx
getElem name = do
  funcIdx <- getFunc name

  modl @ Module { mdElems } <- get
  case elemIndex funcIdx mdElems of
    Nothing -> do
      put modl { mdElems = mdElems ++ [funcIdx] }
      return (fromIntegral $ tableOffset + length mdElems)
    Just elemIdx ->
      return (fromIntegral $ tableOffset + elemIdx)

putCode :: [ValueType] -> [Instruction] -> Generation ()
putCode locals insts = do
  modl @ Module { mdCodes } <- get
  put modl { mdCodes = mdCodes ++ [Code locals insts] }

initialize :: Generation ()
initialize = do
  putImport "object_enter" [I32] []
  putImport "object_leave" [I32] []
  putImport "object_integer" [I32] [I32]
  putImport "object_integer_sum" [I32, I32] [I32]
  putImport "object_real" [F32] [I32]
  putImport "object_real_sum" [I32, I32] [I32]
  
  putImport "object_closure_0"
    [I32] [I32]
  
  putImport "object_closure_1"
    [I32, I32] [I32]
  
  putImport "object_closure_2"
    [I32, I32, I32] [I32]
  
  putImport "object_closure_3"
    [I32, I32, I32, I32] [I32]
  
  putImport "object_closure_4"
    [I32, I32, I32, I32, I32] [I32]
  
  putImport "object_closure_5"
    [I32, I32, I32, I32, I32, I32] [I32]
  
  putImport "object_closure_6"
    [I32, I32, I32, I32, I32, I32, I32] [I32]
  
  putImport "object_closure_7"
    [I32, I32, I32, I32, I32, I32, I32, I32] [I32]
  
  putImport "object_closure_8"
    [I32, I32, I32, I32, I32, I32, I32, I32, I32] [I32]
  
  putImport "object_closure_9"
    [I32, I32, I32, I32, I32, I32, I32, I32, I32, I32] [I32]
  
  putImport "object_apply" [I32, I32] [I32]
  putImport "object_debug" [I32] []

putAbstractionFunc :: Abstraction -> Generation ()
putAbstractionFunc (Abstraction name _ _) =
  putFunc name [I32, I32] [I32]

putDefinitionFunc :: Definition -> Generation ()
putDefinitionFunc (Definition name _) =
  putFunc name [] [I32]

putItemFunc :: Item -> Generation ()
putItemFunc (Item definition abstractions) =
  mapM_ putAbstractionFunc abstractions >> putDefinitionFunc definition

access :: Variable -> [Instruction]
access variable =
  case variable of
    VrEnvironment index ->
      [ LocalGet 0
      , I32Load (fromIntegral $ index * pointerSize) 2
      ]
    VrArgument ->
      [ LocalGet 1
      ]

accessWith :: FuncIdx -> Variable -> [Instruction]
accessWith funcIdx variable =
  access variable ++ [Call funcIdx]

emitEnter :: Variable -> Generation [Instruction]
emitEnter variable = do
  enterFunc <- getFunc "object_enter"
  return (accessWith enterFunc variable)

emitEnters :: [Variable] -> Generation [Instruction]
emitEnters variables = do
  enterFunc <- getFunc "object_enter"
  return (concatMap (accessWith enterFunc) variables)

emitLeaves :: [Variable] -> Generation [Instruction]
emitLeaves variables = do
  leaveFunc <- getFunc "object_leave"
  return (concatMap (accessWith leaveFunc) variables)

emitTerm :: Term -> Generation [Instruction]
emitTerm term =
  case term of
    TrLiteral (LtInteger integer) -> do
      integerFunc <- getFunc "object_integer"
      return [I32Const $ fromIntegral integer, Call integerFunc]
    TrLiteral (LtReal real) -> do
      realFunc <- getFunc "object_real"
      return [F32Const real, Call realFunc]
    TrPrimitive (PrIntegerSum left right) -> do
      left' <- emitTerm left
      right' <- emitTerm right
      integerSumFunc <- getFunc "object_integer_sum"
      return (left' ++ right' ++ [Call integerSumFunc])
    TrPrimitive (PrRealSum left right) -> do
      left' <- emitTerm left
      right' <- emitTerm right
      realSumFunc <- getFunc "object_real_sum"
      return (left' ++ right' ++ [Call realSumFunc])
    TrReference reference -> do
      referenceFunc <- getFunc reference
      return [Call referenceFunc]
    TrVariable variable -> do
      enterInsts <- emitEnter variable
      return (enterInsts ++ access variable)
    TrClosure name variables -> do
      enterInsts <- emitEnters variables
      closureElem <- getElem name
      closureFunc <- getFunc ("object_closure_" ++ show (length variables))

      let
        elemInsts = [I32Elem closureElem]
        variableInsts = concatMap access variables
        funcInsts = [Call closureFunc]
      
      return (enterInsts ++ elemInsts ++ variableInsts ++ funcInsts)
    TrApplication function argument -> do
      function' <- emitTerm function
      argument' <- emitTerm argument
      applyFunc <- getFunc "object_apply"
      return (function' ++ argument' ++ [Call applyFunc])

putAbstractionCode :: Abstraction -> Generation ()
putAbstractionCode (Abstraction _ size term) = do
  insts <- emitTerm term
  leaveInsts <- emitLeaves (variablesFromSize size)
  putCode [] (insts ++ leaveInsts)

putDefinitionCode :: Definition -> Generation ()
putDefinitionCode (Definition _ term) = do
  insts <- emitTerm term
  putCode [] insts 

putItemCode :: Item -> Generation ()
putItemCode (Item definition abstractions) =
  mapM_ putAbstractionCode abstractions >> putDefinitionCode definition

putItems :: [Item] -> Generation ()
putItems items =
  mapM_ putItemFunc items >> mapM_ putItemCode items

putMain :: Generation ()
putMain = do
  mainDefFunc <- getFunc "main$def"
  debugFunc <- getFunc "object_debug"
  leaveFunc <- getFunc "object_leave"

  putFunc "main" [] [I32]

  putCode [I32]
    [ Call mainDefFunc
    , LocalSet 0
    , LocalGet 0
    , Call debugFunc
    , LocalGet 0
    , Call leaveFunc
    , I32Const 0
    ]

generate :: [Item] -> Module
generate items =
  runGeneration (initialize >> putItems items >> putMain)
