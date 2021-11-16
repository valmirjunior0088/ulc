module Ulc.WebAssembly.Serialization
  ( serialize
  )
  where

import Ulc.WebAssembly.Utf8 (utf8)
import Data.Word (Word8, Word32)
import Data.ByteString.Builder (Builder, stringUtf8, int32LE)

import Ulc.WebAssembly.Module
  ( TypeIdx (..)
  , FuncIdx (..)
  , LocalIdx (..)
  , TableIdx (..)
  , MemIdx (..)
  , GlobalIdx (..)
  , DataIdx (..)
  , SecIdx (..)
  , SymIdx (..)

  , NumberType (..)
  , RefType (..)
  , ValueType (..)
  , ResultType (..)
  , FuncType (..)
  , TypeSec (..)

  , Limits (..)
  , TableType (..)
  , MemType (..)
  , Mutability (..)
  , GlobalType (..)
  , ImportDesc (..)
  , Import (..)
  , ImportSec (..)

  , Func (..)
  , FuncSec (..)

  , ExportDesc (..)
  , Export (..)
  , ExportSec (..)

  , Elem (..)
  , ElemSec (..)

  , Locals (..)
  , MemArg (..)
  , Instr (..)
  , Expr (..)
  , Code (..)
  , CodeSec (..)

  , SymKind (..)
  , SymFlag (..)
  , SymInfo (..)
  , SymTable (..)
  , LinkSec (..)

  , Module (..)
  
  , RelocType (..)
  , RelocEntry (..)
  , RelocSec (..)
  )

import Ulc.WebAssembly.Buffer
  ( Buffer (..)
  , byte
  , unsigned
  , unsignedFixed
  , signed
  , signedFixed
  , floatingSingle
  , floatingDouble
  , prependSize
  )

import Ulc.WebAssembly.RelocBuffer
  ( RelocBuffer (..)
  , relocPrependSize
  )

preamble :: Builder
preamble =
  magic <> version where
    magic = stringUtf8 "\0asm"
    version = int32LE 1

class Bufferable a where
  buffered :: a -> Buffer

bufferedVec :: Bufferable a => [a] -> Buffer
bufferedVec values =
  unsigned quantity <> contents where
    quantity = fromIntegral (length values) :: Word32
    contents = mconcat (map buffered values)

instance Bufferable Word8 where
  buffered = byte

bufferedName :: String -> Buffer
bufferedName string =
  bufferedVec (utf8 string)

class RelocBufferable a where
  relocBuffered :: a -> RelocBuffer

relocBufferedVec :: RelocBufferable a => [a] -> RelocBuffer
relocBufferedVec values =
  RelocBuffer (unsigned quantity) [] <> contents where
    quantity = fromIntegral (length values) :: Word32
    contents = mconcat (map relocBuffered values)

instance Bufferable TypeIdx where
  buffered (TypeIdx typeIdx) =
    unsigned typeIdx

instance Bufferable FuncIdx where
  buffered (FuncIdx funcIdx) =
    unsigned funcIdx

instance Bufferable LocalIdx where
  buffered (LocalIdx localIdx) =
    unsigned localIdx

instance Bufferable TableIdx where
  buffered (TableIdx tableIdx) =
    unsigned tableIdx

instance Bufferable MemIdx where
  buffered (MemIdx memIdx) =
    unsigned memIdx

instance Bufferable GlobalIdx where
  buffered (GlobalIdx globalIdx) =
    unsigned globalIdx

instance Bufferable DataIdx where
  buffered (DataIdx dataIdx) =
    unsigned dataIdx

instance Bufferable SecIdx where
  buffered (SecIdx secIdx) =
    unsigned secIdx

instance Bufferable SymIdx where
  buffered (SymIdx symIdx) =
    unsigned symIdx

instance Bufferable NumberType where
  buffered numberType =
    case numberType of
      NtI32 -> byte 0x7F
      NtI64 -> byte 0x7E
      NtF32 -> byte 0x7D
      NtF64 -> byte 0x7C

instance Bufferable RefType where
  buffered refType =
    case refType of
      RfFuncRef -> byte 0x70
      RfExternRef -> byte 0x6F

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
    byte 0x60 <> buffered inputs <> buffered outputs

instance Bufferable TypeSec where
  buffered (TypeSec types) =
    byte 1 <> prependSize (bufferedVec types)

instance Bufferable Limits where
  buffered limits =
    case limits of
      LmUnbounded minSize ->
        byte 0x00 <> unsigned minSize
      LmBounded minSize maxSize ->
        byte 0x01 <> unsigned minSize <> unsigned maxSize

instance Bufferable TableType where
  buffered (TableType refType limits) =
    buffered refType <> buffered limits

instance Bufferable MemType where
  buffered (MemType limits) =
    buffered limits

instance Bufferable Mutability where
  buffered mutability =
    case mutability of
      MtConst -> byte 0x00
      MtVar -> byte 0x01

instance Bufferable GlobalType where
  buffered (GlobalType valueType mutability) =
    buffered valueType <> buffered mutability

instance Bufferable ImportDesc where
  buffered imptDesc =
    case imptDesc of
      IdFunc typeIdx -> byte 0x00 <> buffered typeIdx
      IdTable tableType -> byte 0x01 <> buffered tableType
      IdMem memType -> byte 0x02 <> buffered memType
      IdGlobal globalType -> byte 0x03 <> buffered globalType

instance Bufferable Import where
  buffered (Import name namespace imptDesc) =
    bufferedName name <> bufferedName namespace <> buffered imptDesc

instance Bufferable ImportSec where
  buffered (ImportSec imports) =
    byte 2 <> prependSize (bufferedVec imports)

instance Bufferable Func where
  buffered (Func _ typeIdx) =
    buffered typeIdx

instance Bufferable FuncSec where
  buffered (FuncSec funcs) =
    byte 3 <> prependSize (bufferedVec funcs)

instance Bufferable ExportDesc where
  buffered exportDesc =
    case exportDesc of
      EdFunc funcIdx -> byte 0 <> buffered funcIdx
      EdTable tableIdx -> byte 1 <> buffered tableIdx
      EdMem memIdx -> byte 2 <> buffered memIdx
      EdGlobal globalIdx -> byte 3 <> buffered globalIdx

instance Bufferable Export where
  buffered (Export name exportDesc) =
    bufferedName name <> buffered exportDesc

instance Bufferable ExportSec where
  buffered (ExportSec exports) =
    byte 7 <> prependSize (bufferedVec exports)

instance Bufferable Elem where
  buffered (Elem offsetExpr funcIdxs) =
    byte 0x00
      <> let RelocBuffer buffer _ = relocBuffered offsetExpr in buffer
      <> bufferedVec funcIdxs

instance Bufferable ElemSec where
  buffered (ElemSec elems) =
    byte 9 <> prependSize (bufferedVec elems)

instance Bufferable Locals where
  buffered (Locals quantity valueType) =
    unsigned quantity <> buffered valueType

instance Bufferable MemArg where
  buffered (MemArg alignment offset) =
    unsigned alignment <> unsigned offset

instance RelocBufferable Instr where
  relocBuffered instr =
    case instr of
      InI32Const value ->
        RelocBuffer (byte 0x41 <> signed value) []

      InI32FuncRef value symIdx ->
        relocInstr <> relocValue where
          relocInstr = RelocBuffer (byte 0x41) []
          relocEntry = RelocEntry RlTableIndexSleb 0 symIdx
          relocValue = RelocBuffer (signedFixed 5 value) [relocEntry]

      InI64Const value ->
        RelocBuffer (byte 0x42 <> signed value) []

      InF32Const value ->
        RelocBuffer (byte 0x43 <> floatingSingle value) []

      InF64Const value ->
        RelocBuffer (byte 0x44 <> floatingDouble value) []

      InI32Load memArg ->
        RelocBuffer (byte 0x28 <> buffered memArg) []

      InLocalGet localIdx ->
        RelocBuffer (byte 0x20 <> buffered localIdx) []

      InLocalSet localIdx ->
        RelocBuffer (byte 0x21 <> buffered localIdx) []

      InLocalTee localIdx ->
        RelocBuffer (byte 0x22 <> buffered localIdx) []

      InCall (FuncIdx funcIdx) symIdx ->
        relocInstr <> relocValue where
          relocInstr = RelocBuffer (byte 0x10) []
          relocEntry = RelocEntry RlFunctionIndexLeb 0 symIdx
          relocValue = RelocBuffer (unsignedFixed 5 funcIdx) [relocEntry]

instance RelocBufferable Expr where
  relocBuffered (Expr instrs) =
    mconcat (map relocBuffered instrs) <> RelocBuffer (byte 0x0B) []

instance RelocBufferable Code where
  relocBuffered (Code locals expr) =
    relocPrependSize (RelocBuffer (bufferedVec locals) [] <> relocBuffered expr)

instance RelocBufferable CodeSec where
  relocBuffered (CodeSec codes) =
    RelocBuffer (byte 10 <> prependSize buffer) entries where
      RelocBuffer buffer entries = relocBufferedVec codes

packSymFlags :: [SymFlag] -> Word32
packSymFlags flags =
  sum (map go flags) where
    go flag =
      case flag of
        SfVisibilityHidden -> 0x04
        SfUndefined -> 0x10
        SfExported -> 0x20
        SfExplicitName -> 0x40

instance Bufferable SymInfo where
  buffered (SymInfo kind flags) =
    case kind of
      SkFunction funcIdx name ->
        byte 0
          <> unsigned (packSymFlags flags)
          <> buffered funcIdx
          <> maybe mempty bufferedName name

      SkData name dataIdx offset size ->
        byte 1
          <> bufferedName name
          <> maybe mempty buffered dataIdx
          <> maybe mempty unsigned offset
          <> maybe mempty unsigned size
        
instance Bufferable SymTable where
  buffered (SymTable symInfos) =
    byte 8 <> prependSize (bufferedVec symInfos)

instance Bufferable LinkSec where
  buffered (LinkSec symTable) =
    byte 0 <> prependSize contents where
      name = bufferedName "linking" 
      version = unsigned (2 :: Word32)
      contents = name <> version <> buffered symTable

instance Bufferable RelocType where
  buffered relocType =
    case relocType of
      RlFunctionIndexLeb -> byte 0
      RlTableIndexSleb -> byte 1

instance Bufferable RelocEntry where
  buffered (RelocEntry relocType offset symIdx) =
    buffered relocType <> unsigned offset <> buffered symIdx

instance Bufferable RelocSec where
  buffered (RelocSec suffix secIdx entries) =
    byte 0 <> prependSize contents where
      name = bufferedName ("reloc." ++ suffix)
      contents = name <> buffered secIdx <> bufferedVec entries

instance Bufferable Module where
  buffered modl =
    let
      Module
        { mdTypes = typeSec
        , mdImports = importSec
        , mdFuncs = funcSec
        , mdExports = exportSec
        , mdElems = elemSec
        , mdCodes = codeSec
        , mdLink = linkSec
        }
        = modl
      
      typeBuffer = buffered typeSec -- 0
      importBuffer = buffered importSec -- 1
      funcBuffer = buffered funcSec -- 2
      exportBuffer = buffered exportSec -- 3
      elemBuffer = buffered elemSec -- 4
      RelocBuffer codeBuffer codeRelocs = relocBuffered codeSec -- 5
      linkBuffer = buffered linkSec -- 6
      codeRelocBuffer = buffered (RelocSec "CODE" 5 codeRelocs) -- 7
    in
      typeBuffer
        <> importBuffer
        <> funcBuffer
        <> exportBuffer
        <> elemBuffer
        <> codeBuffer
        <> linkBuffer
        <> codeRelocBuffer

serialize :: Module -> Builder
serialize modl =
  let Buffer _ builder = buffered modl in preamble <> builder
