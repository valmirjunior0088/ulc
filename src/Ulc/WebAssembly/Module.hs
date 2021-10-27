{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ulc.WebAssembly.Module
  ( TypeIdx (..)
  , FuncIdx (..)
  , LocalIdx (..)

  , NumberType (..)
  , RefType (..)
  , ValueType (..)
  , i32
  , i64
  , f32
  , f64
  , funcRef
  , externRef
  , ResultType (..)
  , FuncType (..)
  , TypeSec (..)
  , emptyTypeSec

  , MinSize
  , MaxSize
  , Limits (..)
  , TableType (..)
  , MemoryType (..)
  , Mutability (..)
  , GlobalType (..)
  , ImportDesc (..)
  , Import (..)
  , ImportSec (..)
  , emptyImportSec

  , Func (..)
  , FuncSec (..)
  , emptyFuncSec

  , Elem (..)
  , ElemSec (..)
  , emptyElemSec

  , Locals (..)
  , buildLocals
  , Offset
  , Alignment
  , MemArg (..)
  , Instr (..)
  , Expr (..)
  , Code (..)
  , CodeSec (..)
  , emptyCodeSec

  , Module (..)
  , emptyModule

  , tableOffset
  , pointerSize
  )
  where

import Data.Int (Int32, Int64)
import Data.Word (Word32)
import Data.List (groupBy)

newtype TypeIdx =
  TypeIdx Word32
  deriving (Num, Show)

newtype FuncIdx =
  FuncIdx Word32
  deriving (Num, Eq, Show)

newtype LocalIdx =
  LocalIdx Word32
  deriving (Num, Show)

data NumberType =
  NtI32 |
  NtI64 |
  NtF32 |
  NtF64
  deriving (Eq, Show)

data RefType =
  RtFuncRef |
  RtExternRef
  deriving (Eq, Show)

data ValueType =
  VtNumberType NumberType |
  VtRefType RefType
  deriving (Eq, Show)

i32 :: ValueType
i32 = VtNumberType NtI32

i64 :: ValueType
i64 = VtNumberType NtI64

f32 :: ValueType
f32 = VtNumberType NtF32

f64 :: ValueType
f64 = VtNumberType NtF64

funcRef :: ValueType
funcRef = VtRefType RtFuncRef

externRef :: ValueType
externRef = VtRefType RtExternRef

newtype ResultType =
  ResultType [ValueType]
  deriving (Eq, Show)

data FuncType =
  FuncType ResultType ResultType
  deriving (Eq, Show)

newtype TypeSec =
  TypeSec [FuncType]
  deriving (Show)

emptyTypeSec :: TypeSec
emptyTypeSec = TypeSec []

type MinSize = Word32
type MaxSize = Word32

data Limits =
  LmUnbounded MinSize |
  LmBounded MinSize MaxSize
  deriving (Show)

data TableType =
  TableType RefType Limits
  deriving (Show)

newtype MemoryType =
  MemoryType Limits
  deriving (Show)

data Mutability =
  MtConst |
  MtVar
  deriving (Show)

data GlobalType =
  GlobalType ValueType Mutability
  deriving (Show)

data ImportDesc =
  IdFunc TypeIdx |
  IdTable TableType |
  IdMemory MemoryType |
  IdGlobal GlobalType
  deriving (Show)

data Import =
  Import String String ImportDesc
  deriving (Show)

newtype ImportSec =
  ImportSec [Import]
  deriving (Show)

emptyImportSec :: ImportSec
emptyImportSec = ImportSec []

data Func =
  Func String [String] TypeIdx
  deriving (Show)

newtype FuncSec =
  FuncSec [Func]
  deriving (Show)

emptyFuncSec :: FuncSec
emptyFuncSec = FuncSec []

data Elem =
  Elem Expr [FuncIdx]
  deriving (Show)

newtype ElemSec =
  ElemSec [Elem]
  deriving (Show)

emptyElemSec :: ElemSec
emptyElemSec = ElemSec []

data Locals =
  Locals [String] ValueType
  deriving (Show)

buildLocals :: [(String, ValueType)] -> [Locals]
buildLocals =
  map transform . groupBy predicate where
    predicate (_, valueType) (_, valueType') = valueType == valueType'
    transform locals = Locals (map fst locals) (snd $ head locals)

type Alignment = Word32
type Offset = Word32

data MemArg =
  MemArg Alignment Offset
  deriving (Show)

data Instr =
  InI32Const Int32 |
  InI32Load MemArg |
  InI64Const Int64 |
  InF32Const Float |
  InF64Const Double |
  InLocalGet LocalIdx |
  InLocalSet LocalIdx |
  InLocalTee LocalIdx |
  InCall FuncIdx
  deriving (Show)

newtype Expr =
  Expr [Instr]
  deriving (Show)

data Code =
  Code [Locals] Expr
  deriving (Show)

newtype CodeSec =
  CodeSec [Code]
  deriving (Show)

emptyCodeSec :: CodeSec
emptyCodeSec = CodeSec []

data Module =
  Module
    { mdTypes :: TypeSec
    , mdImports :: ImportSec
    , mdFuncs :: FuncSec
    , mdElems :: ElemSec
    , mdCodes :: CodeSec
    }
  deriving (Show)

emptyModule :: Module
emptyModule =
  Module
    { mdTypes = emptyTypeSec
    , mdImports = emptyImportSec
    , mdFuncs = emptyFuncSec
    , mdElems = emptyElemSec
    , mdCodes = emptyCodeSec
    }

-- Elem 0 is reserved for the null function pointer
tableOffset :: Int
tableOffset = 1

-- Wasm32 has a pointer size of 4 bytes
pointerSize :: Int
pointerSize = 4
