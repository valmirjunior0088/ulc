module Ulc.C.PrettyPrinting
  ( annotated
  , pretty
  )
  where

import Ulc.C.Generation (Instruction (..), Function (..), Item (..))
import Data.List (intercalate)

annotated :: String -> String
annotated string =
  "struct object *" ++ string

indent :: [String] -> [String]
indent =
  map ("  " ++)

prettyInstruction :: Instruction -> String
prettyInstruction instruction =
  case instruction of
    InEnter object ->
      "object_enter(" ++ object ++ ");"
    InLeave object ->
      "object_leave(" ++ object ++ ");"
    InInteger tmp value ->
      annotated tmp
        ++ " = object_integer("
        ++ show value
        ++ ");"
    InIntegerSum tmp left right ->
      annotated tmp
        ++ " = object_integer_sum("
        ++ left ++ ", " ++ right
        ++ ");"
    InReal tmp value ->
      annotated tmp
        ++ " = object_real("
        ++ show value
        ++ ");"
    InRealSum tmp left right ->
      annotated tmp
        ++ " = object_real_sum("
        ++ left ++ ", " ++ right
        ++ ");"
    InClosure tmp name variables ->
      if null variables
        then prefix ++ suffix
        else prefix ++ ", " ++ content ++ suffix
      where
        prefix =
          annotated tmp
            ++ " = object_closure_" ++ show (length variables)
            ++ "(" ++ name
        content =
          intercalate ", " variables
        suffix =
          ");"
    InApply tmp function argument ->
      annotated tmp
        ++ " = object_apply("
        ++ function ++ ", " ++ argument
        ++ ");"
    InReference tmp name ->
      annotated tmp ++ " = " ++ name ++ "();"
    InReturn object ->
      "return " ++ object ++ ";"
    InNewLine ->
      ""

prettyArguments :: [String] -> String
prettyArguments arguments =
  "(" ++ intercalate ", " (map annotated arguments) ++ ")"

prettyFunction :: String -> [String] -> [Instruction] -> [String]
prettyFunction name arguments instructions =
  [annotated name ++ prettyArguments arguments ++ " {"]
    ++ indent (map prettyInstruction instructions)
    ++ ["}", ""]

prettyAbstraction :: Function -> [String]
prettyAbstraction (Function name instructions) =
  prettyFunction name ["env[]", "arg"] instructions

prettyDefinition :: Function -> [String]
prettyDefinition (Function name instructions) =
  prettyFunction name [] instructions

pretty :: Item -> [String]
pretty (Item definition abstractions) =
  concat (map prettyAbstraction abstractions) ++ prettyDefinition definition
