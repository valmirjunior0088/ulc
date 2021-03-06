module Ulc.WebAssembly.Generation
  ( generate
  )
  where

import Ulc.Common
  ( Variable (..)
  , Literal (..)
  , Primitive (..)
  , Term (..)
  , Abstraction (..)
  , Definition (..)
  , Item (..)
  , variablesFromSize
  )

import Ulc.WebAssembly.Module
  ( i32
  , f32
  , RefType (..)
  , Limits (..)
  , Instr (..)
  , Module (..)
  )

import Ulc.WebAssembly.Syntax
  ( Syntax
  , runSyntax
  , putTableImport
  , putMemImport
  , putCode
  , getFuncRefsLength
  , i32Const
  , i32FuncRef
  , f32Const
  , i32Load
  , localGet
  , localTee
  , call
  , addFuncImport
  , addFunc
  , addExportedFunc
  )

-- Wasm32 has a pointer size of 4 bytes
pointerSize :: Int
pointerSize = 4

generateRuntimeFuncImports :: Syntax ()
generateRuntimeFuncImports = do
  addFuncImport "env" "object_enter" [i32] []
  addFuncImport "env" "object_leave" [i32] []
  addFuncImport "env" "object_integer" [i32] [i32]
  addFuncImport "env" "object_integer_sum" [i32, i32] [i32]
  addFuncImport "env" "object_real" [f32] [i32]
  addFuncImport "env" "object_real_sum" [i32, i32] [i32]
  
  addFuncImport "env" "object_closure_0"
    [i32] [i32]
  
  addFuncImport "env" "object_closure_1"
    [i32, i32] [i32]
  
  addFuncImport "env" "object_closure_2"
    [i32, i32, i32] [i32]
  
  addFuncImport "env" "object_closure_3"
    [i32, i32, i32, i32] [i32]
  
  addFuncImport "env" "object_closure_4"
    [i32, i32, i32, i32, i32] [i32]
  
  addFuncImport "env" "object_closure_5"
    [i32, i32, i32, i32, i32, i32] [i32]
  
  addFuncImport "env" "object_closure_6"
    [i32, i32, i32, i32, i32, i32, i32] [i32]
  
  addFuncImport "env" "object_closure_7"
    [i32, i32, i32, i32, i32, i32, i32, i32] [i32]
  
  addFuncImport "env" "object_closure_8"
    [i32, i32, i32, i32, i32, i32, i32, i32, i32] [i32]
  
  addFuncImport "env" "object_closure_9"
    [i32, i32, i32, i32, i32, i32, i32, i32, i32, i32] [i32]
  
  addFuncImport "env" "object_apply" [i32, i32] [i32]
  addFuncImport "env" "object_debug" [i32] []

generateAbstractionFunc :: Abstraction -> Syntax ()
generateAbstractionFunc (Abstraction name _ _) = do
  addFunc name [i32, i32] [i32]

generateDefinitionFunc :: Definition -> Syntax ()
generateDefinitionFunc (Definition name _) =
  addFunc name [] [i32]

generateItemFunc :: Item -> Syntax ()
generateItemFunc (Item definition abstractions) = do
  mapM_ generateAbstractionFunc abstractions
  generateDefinitionFunc definition

generateItemsFuncs :: [Item] -> Syntax ()
generateItemsFuncs =
  mapM_ generateItemFunc

access :: Variable -> Syntax [Instr]
access variable =
  case variable of
    VrEnvironment index ->
      sequence [localGet localIdx, i32Load alignment offset] where
        localIdx = 0
        alignment = 2
        offset = fromIntegral (index * pointerSize)
    VrArgument ->
      sequence [localGet localIdx] where
        localIdx = 1

emitEnter :: Variable -> Syntax [Instr]
emitEnter variable = do
  accessIntrs <- access variable
  callInstrs <- return <$> call "object_enter"
  return (accessIntrs ++ callInstrs)

emitEnters :: [Variable] -> Syntax [Instr]
emitEnters variables = do
  concat <$> mapM emitEnter variables

emitLeave :: Variable -> Syntax [Instr]
emitLeave variable = do
  leaveInstrs <- access variable
  callInstrs <- return <$> call "object_leave"
  return (leaveInstrs ++ callInstrs)

emitLeaves :: [Variable] -> Syntax [Instr]
emitLeaves variables = do
  concat <$> mapM emitLeave variables

emitTerm :: Term -> Syntax [Instr]
emitTerm term =
  case term of
    TrLiteral (LtInteger value) -> do
      constInstrs <- return <$> i32Const (fromIntegral value)
      callInstrs <- return <$> call "object_integer"
      return (constInstrs ++ callInstrs)

    TrLiteral (LtReal value) -> do
      constInstrs <- return <$> f32Const value
      callInstrs <- return <$> call "object_real"
      return (constInstrs ++ callInstrs)

    TrPrimitive (PrIntegerSum left right) -> do
      leftInstrs <- emitTerm left
      rightInstrs <- emitTerm right
      callInstrs <- return <$> call "object_integer_sum"
      return (leftInstrs ++ rightInstrs ++ callInstrs)

    TrPrimitive (PrRealSum left right) -> do
      leftInstrs <- emitTerm left
      rightInstrs <- emitTerm right
      callInstrs <- return <$> call "object_real_sum"
      return (leftInstrs ++ rightInstrs ++ callInstrs)

    TrReference reference ->
      return <$> call reference

    TrVariable variable -> do
      enterInstrs <- emitEnter variable
      accessInstrs <- access variable
      return (enterInstrs ++ accessInstrs)

    TrClosure name variables -> do
      enterInstrs <- emitEnters variables
      funcRefInstrs <- return <$> i32FuncRef name
      variablesInstrs <- concat <$> mapM access variables

      let funcName = "object_closure_" ++ show (length variables)
      callInstrs <- return <$> call funcName
      
      return (enterInstrs ++ funcRefInstrs ++ variablesInstrs ++ callInstrs)

    TrApplication function argument -> do
      functionInstrs <- emitTerm function
      argumentInstrs <- emitTerm argument
      callInstrs <- return <$> call "object_apply"
      return (functionInstrs ++ argumentInstrs ++ callInstrs)

generateAbstractionCode :: Abstraction -> Syntax ()
generateAbstractionCode (Abstraction _ size term) = do
  instrs <- emitTerm term
  leaveInstrs <- emitLeaves (variablesFromSize size)
  putCode [] (instrs ++ leaveInstrs)

generateDefinitionCode :: Definition -> Syntax ()
generateDefinitionCode (Definition _ term) = do
  instrs <- emitTerm term
  putCode [] instrs

generateItemCode :: Item -> Syntax ()
generateItemCode (Item definition abstractions) = do
  mapM_ generateAbstractionCode abstractions
  generateDefinitionCode definition

generateItemsCodes :: [Item] -> Syntax ()
generateItemsCodes =
  mapM_ generateItemCode

generateRuntimeTableMemImports :: Syntax ()
generateRuntimeTableMemImports = do
  tableLimits <- LmUnbounded . fromIntegral <$> getFuncRefsLength
  putTableImport "env" "__indirect_function_table" RfFuncRef tableLimits
  putMemImport "env" "__linear_memory" (LmUnbounded 1)

generateMainFunc :: Syntax ()
generateMainFunc =
  addExportedFunc "main" [] [i32]

generateMainCode :: Syntax ()
generateMainCode = do
  instrs <- sequence
    [ call "main$def"
    , localTee 0
    , call "object_debug"
    , localGet 0
    , call "object_leave"
    , i32Const 0
    ]
  
  putCode [i32] instrs

generateProgram :: [Item] -> Syntax ()
generateProgram items = do
  generateRuntimeFuncImports

  generateItemsFuncs items
  generateMainFunc

  generateItemsCodes items
  generateMainCode

  generateRuntimeTableMemImports

generate :: [Item] -> Module
generate items =
  runSyntax (generateProgram items)
