module Ulc.WebAssembly.Serialization
  ( serialize
  )
  where

import Ulc.WebAssembly.Leb128 (uleb128)
import Ulc.WebAssembly.Utf8 (utf8)
import Ulc.WebAssembly.Buffer (Buffer (..))
import qualified Ulc.WebAssembly.Buffer as Buffer
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import Data.Word (Word8, Word32)

import Ulc.WebAssembly.Module
  ( TypeIdx (..)
  , FuncIdx (..)
  , LocalIdx (..)

  , NumberType (..)
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

  , Locals (..)
  , MemArg (..)
  , Instr (..)
  , Expr (..)
  , Code (..)
  , CodeSec (..)

  , Module (..)
  )

class Bufferable a where
  buffered :: a -> Buffer

bufferedVec :: Bufferable a => [a] -> Buffer
bufferedVec values =
  Buffer.unsigned quantity <> contents where
    quantity = fromIntegral (length values) :: Word32
    contents = mconcat (map buffered values)

instance Bufferable Word8 where
  buffered = Buffer.byte

bufferedName :: String -> Buffer
bufferedName string =
  bufferedVec (utf8 string)

prependSize :: Buffer -> Buffer
prependSize buffer @ (Buffer size _) =
  Buffer.unsigned size <> buffer

instance Bufferable TypeIdx where
  buffered (TypeIdx typeIdx) =
    Buffer.unsigned typeIdx

instance Bufferable FuncIdx where
  buffered (FuncIdx funcIdx) =
    Buffer.unsigned funcIdx

instance Bufferable LocalIdx where
  buffered (LocalIdx localIdx) =
    Buffer.unsigned localIdx

instance Bufferable NumberType where
  buffered numberType =
    case numberType of
      NtI32 -> Buffer.byte 0x7F
      NtI64 -> Buffer.byte 0x7E
      NtF32 -> Buffer.byte 0x7D
      NtF64 -> Buffer.byte 0x7C

instance Bufferable RefType where
  buffered refType =
    case refType of
      RtFuncRef -> Buffer.byte 0x70
      RtExternRef -> Buffer.byte 0x6F

instance Bufferable ValueType where
  buffered valueType =
    case valueType of
      VtNumberType numberType -> buffered numberType
      VtRefType refType -> buffered refType

instance Bufferable ResultType where
  buffered (ResultType valueTypes) =
    bufferedVec valueTypes

instance Bufferable FuncType where
  buffered (FuncType inputs outputs) =
    Buffer.byte 0x60 <> buffered inputs <> buffered outputs

instance Bufferable TypeSec where
  buffered (TypeSec types) =
    bufferedVec types

instance Bufferable Limits where
  buffered limits =
    case limits of
      LmUnbounded minSize ->
        Buffer.byte 0x00 <> Buffer.unsigned minSize
      LmBounded minSize maxSize ->
        Buffer.byte 0x01 <> Buffer.unsigned minSize <> Buffer.unsigned maxSize

instance Bufferable TableType where
  buffered (TableType refType limits) =
    buffered refType <> buffered limits

instance Bufferable MemoryType where
  buffered (MemoryType limits) =
    buffered limits

instance Bufferable Mutability where
  buffered mutability =
    case mutability of
      MtConst -> Buffer.byte 0x00
      MtVar -> Buffer.byte 0x01

instance Bufferable GlobalType where
  buffered (GlobalType valueType mutability) =
    buffered valueType <> buffered mutability

instance Bufferable ImportDesc where
  buffered imptDesc =
    case imptDesc of
      IdFunc typeIdx -> Buffer.byte 0x00 <> buffered typeIdx
      IdTable tableType -> Buffer.byte 0x01 <> buffered tableType
      IdMemory memoryType -> Buffer.byte 0x02 <> buffered memoryType
      IdGlobal globalType -> Buffer.byte 0x03 <> buffered globalType

instance Bufferable Import where
  buffered (Import name namespace imptDesc) =
    bufferedName name <> bufferedName namespace <> buffered imptDesc

instance Bufferable ImportSec where
  buffered (ImportSec imports) =
    bufferedVec imports

instance Bufferable Func where
  buffered (Func _ _ typeIdx) =
    buffered typeIdx

instance Bufferable FuncSec where
  buffered (FuncSec funcs) =
    bufferedVec funcs

instance Bufferable Elem where
  buffered (Elem offsetExpr funcIdxs) =
    Buffer.byte 0x00 <> buffered offsetExpr <> bufferedVec funcIdxs

instance Bufferable ElemSec where
  buffered (ElemSec elems) =
    bufferedVec elems

instance Bufferable Locals where
  buffered (Locals names valueType) =
    Buffer.unsigned quantity <> buffered valueType where
      quantity = fromIntegral (length names) :: Word32

instance Bufferable MemArg where
  buffered (MemArg offset alignment) =
    Buffer.unsigned offset <> Buffer.unsigned alignment

instance Bufferable Instr where
  buffered instr =
    case instr of
      InI32Const value -> Buffer.byte 0x41 <> Buffer.signed value
      InI32Load memArg -> Buffer.byte 0x28 <> buffered memArg
      InI64Const value -> Buffer.byte 0x42 <> Buffer.signed value
      InF32Const value -> Buffer.byte 0x43 <> Buffer.floatingSingle value
      InF64Const value -> Buffer.byte 0x44 <> Buffer.floatingDouble value
      InLocalGet localIdx -> Buffer.byte 0x20 <> buffered localIdx
      InLocalSet localIdx -> Buffer.byte 0x21 <> buffered localIdx
      InLocalTee localIdx -> Buffer.byte 0x22 <> buffered localIdx
      InCall funcIdx -> Buffer.byte 0x10 <> buffered funcIdx

instance Bufferable Expr where
  buffered (Expr instrs) =
    mconcat (map buffered instrs) <> Buffer.byte 0x0B

instance Bufferable Code where
  buffered (Code locals expr) =
    prependSize (bufferedVec locals <> buffered expr)

instance Bufferable CodeSec where
  buffered (CodeSec codes) =
    bufferedVec codes

preamble :: Builder
preamble =
  magic <> version where
    magic = Builder.stringUtf8 "\0asm"
    version = Builder.int32LE 1

section :: Bufferable a => Word8 -> a -> Builder
section identifier bufferable =
  Builder.word8 identifier
    <> mconcat (map Builder.word8 $ uleb128 size)
    <> builder
  where
    Buffer size builder =  buffered bufferable

serialize :: Module -> Builder
serialize modl =
  let
    Module
      { mdTypes = typeSec
      , mdImports = imptSec
      , mdFuncs = funcSec
      , mdElems = elemSec
      , mdCodes = codeSec
      }
      = modl
  in
    preamble
      <> section 01 typeSec
      <> section 02 imptSec
      <> section 03 funcSec
      <> section 09 elemSec
      <> section 10 codeSec
