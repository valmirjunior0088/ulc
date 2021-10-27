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
  ( FuncIdx (..)
  , LocalIdx (..)
  , i32
  , f32
  , RefType (..)
  , Limits (..)
  , MemArg (..)
  , Instr (..)
  , Module (..)
  , pointerSize
  )

import Ulc.WebAssembly.Syntax
  ( Syntax
  , runSyntax
  , importFunc
  , importTable
  , importMemory
  , addFunc
  , addCode
  , getFunc
  , getFuncRef
  , getFuncRefsLength
  )

generateRuntimeFuncImports :: Syntax ()
generateRuntimeFuncImports = do
  importFunc "env" "object_enter" [i32] []
  importFunc "env" "object_leave" [i32] []
  importFunc "env" "object_integer" [i32] [i32]
  importFunc "env" "object_integer_sum" [i32, i32] [i32]
  importFunc "env" "object_real" [f32] [i32]
  importFunc "env" "object_real_sum" [i32, i32] [i32]
  
  importFunc "env" "object_closure_0"
    [i32] [i32]
  
  importFunc "env" "object_closure_1"
    [i32, i32] [i32]
  
  importFunc "env" "object_closure_2"
    [i32, i32, i32] [i32]
  
  importFunc "env" "object_closure_3"
    [i32, i32, i32, i32] [i32]
  
  importFunc "env" "object_closure_4"
    [i32, i32, i32, i32, i32] [i32]
  
  importFunc "env" "object_closure_5"
    [i32, i32, i32, i32, i32, i32] [i32]
  
  importFunc "env" "object_closure_6"
    [i32, i32, i32, i32, i32, i32, i32] [i32]
  
  importFunc "env" "object_closure_7"
    [i32, i32, i32, i32, i32, i32, i32, i32] [i32]
  
  importFunc "env" "object_closure_8"
    [i32, i32, i32, i32, i32, i32, i32, i32, i32] [i32]
  
  importFunc "env" "object_closure_9"
    [i32, i32, i32, i32, i32, i32, i32, i32, i32, i32] [i32]
  
  importFunc "env" "object_apply" [i32, i32] [i32]
  importFunc "env" "object_debug" [i32] []

generateAbstractionFunc :: Abstraction -> Syntax ()
generateAbstractionFunc (Abstraction name _ _) =
  addFunc name [("env", i32), ("arg", i32)] [i32]

generateDefinitionFunc :: Definition -> Syntax ()
generateDefinitionFunc (Definition name _) =
  addFunc name [] [i32]

generateItemFunc :: Item -> Syntax ()
generateItemFunc (Item definition abstractions) =
  mapM_ generateAbstractionFunc abstractions >> generateDefinitionFunc definition

access :: Variable -> [Instr]
access variable =
  case variable of
    VrEnvironment index ->
      [InLocalGet localIdx, InI32Load $ MemArg alignment offset] where
        localIdx = 0
        alignment = 2
        offset = fromIntegral (index * pointerSize)
    VrArgument ->
      [InLocalGet localIdx] where
        localIdx = 1

accessWith :: FuncIdx -> Variable -> [Instr]
accessWith funcIdx variable =
  access variable ++ [InCall funcIdx]

emitEnter :: Variable -> Syntax [Instr]
emitEnter variable = do
  enterFunc <- getFunc "object_enter"
  return (accessWith enterFunc variable)

emitEnters :: [Variable] -> Syntax [Instr]
emitEnters variables = do
  enterFunc <- getFunc "object_enter"
  return (concatMap (accessWith enterFunc) variables)

emitLeaves :: [Variable] -> Syntax [Instr]
emitLeaves variables = do
  leaveFunc <- getFunc "object_leave"
  return (concatMap (accessWith leaveFunc) variables)

emitTerm :: Term -> Syntax [Instr]
emitTerm term =
  case term of
    TrLiteral (LtInteger integer) -> do
      let constInstrs = [InI32Const $ fromIntegral integer]

      func <- getFunc "object_integer"
      let callInstrs = [InCall func]

      return (constInstrs ++ callInstrs)

    TrLiteral (LtReal real) -> do
      let constInstrs = [InF32Const real]

      func <- getFunc "object_real"
      let callInstrs = [InCall func]

      return (constInstrs ++ callInstrs)

    TrPrimitive (PrIntegerSum left right) -> do
      leftInstrs <- emitTerm left
      rightInstrs <- emitTerm right

      func <- getFunc "object_integer_sum"
      let callInstrs = [InCall func]

      return (leftInstrs ++ rightInstrs ++ callInstrs)

    TrPrimitive (PrRealSum left right) -> do
      leftInstrs <- emitTerm left
      rightInstrs <- emitTerm right

      func <- getFunc "object_real_sum"
      let callInstrs = [InCall func]

      return (leftInstrs ++ rightInstrs ++ callInstrs)

    TrReference reference -> do
      func <- getFunc reference
      let callInstrs = [InCall func]

      return callInstrs

    TrVariable variable -> do
      enterInstrs <- emitEnter variable
      return (enterInstrs ++ access variable)

    TrClosure name variables -> do
      enterInstrs <- emitEnters variables

      funcRef <- getFuncRef name
      let funcRefInstrs = [InI32Const funcRef]

      let variableInstrs = concatMap access variables
      
      func <- getFunc $ "object_closure_" ++ show (length variables)
      let callInstrs = [InCall func]
      
      return (enterInstrs ++ funcRefInstrs ++ variableInstrs ++ callInstrs)

    TrApplication function argument -> do
      functionInstrs <- emitTerm function
      argumentInstrs <- emitTerm argument

      func <- getFunc "object_apply"
      let callInstrs = [InCall func]

      return (functionInstrs ++ argumentInstrs ++ callInstrs)

generateAbstractionCode :: Abstraction -> Syntax ()
generateAbstractionCode (Abstraction _ size term) = do
  instrs <- emitTerm term
  leaveInstrs <- emitLeaves (variablesFromSize size)
  addCode [] (instrs ++ leaveInstrs)

generateDefinitionCode :: Definition -> Syntax ()
generateDefinitionCode (Definition _ term) = do
  instrs <- emitTerm term
  addCode [] instrs

generateItemCode :: Item -> Syntax ()
generateItemCode (Item definition abstractions) =
  mapM_ generateAbstractionCode abstractions >> generateDefinitionCode definition

generateItems :: [Item] -> Syntax ()
generateItems items =
  mapM_ generateItemFunc items >> mapM_ generateItemCode items

generateMain :: Syntax ()
generateMain = do
  mainDefFunc <- getFunc "main$def"
  debugFunc <- getFunc "object_debug"
  leaveFunc <- getFunc "object_leave"

  addFunc "main" [] [i32]

  addCode [("object", i32)]
    [ InCall mainDefFunc
    , InLocalTee 0
    , InCall debugFunc
    , InLocalGet 0
    , InCall leaveFunc
    , InI32Const 0
    ]

generateRuntimeTableMemoryImports :: Syntax ()
generateRuntimeTableMemoryImports = do
  funcRefsLength <- getFuncRefsLength

  importTable "env" "__indirect_function_table"
    RtFuncRef (LmUnbounded $ fromIntegral funcRefsLength)
  
  importMemory "env" "__linear_memory" (LmUnbounded 1)

generateProgram :: [Item] -> Syntax ()
generateProgram items = do
  generateRuntimeFuncImports
  generateItems items
  generateRuntimeTableMemoryImports
  generateMain

generate :: [Item] -> Module
generate items =
  runSyntax (generateProgram items)
