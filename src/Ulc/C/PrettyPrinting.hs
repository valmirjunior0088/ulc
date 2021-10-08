module Ulc.C.PrettyPrinting
  (annotated
  ,pretty
  )
  where

import Ulc.C.Generation (Statement (..), Function (..), Item (..))
import Data.List (intercalate)

annotated :: String -> String
annotated string =
  "struct object *" ++ string

indent :: [String] -> [String]
indent =
  map ("  " ++)

prettyStatement :: Statement -> String
prettyStatement statement =
  case statement of
    StEnter object ->
      "object_enter(" ++ object ++ ");"
    StLeave object ->
      "object_leave(" ++ object ++ ");"
    StInteger tmp value ->
      annotated tmp
        ++ " = object_integer("
        ++ show value
        ++ ");"
    StIntegerSum tmp left right ->
      annotated tmp
        ++ " = object_integer_sum("
        ++ left ++ ", " ++ right
        ++ ");"
    StReal tmp value ->
      annotated tmp
        ++ " = object_real("
        ++ show value
        ++ ");"
    StRealSum tmp left right ->
      annotated tmp
        ++ " = object_real_sum("
        ++ left ++ ", " ++ right
        ++ ");"
    StClosure tmp name variables ->
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
    StApply tmp function argument ->
      annotated tmp
        ++ " = object_apply("
        ++ function ++ ", " ++ argument
        ++ ");"
    StReference tmp name ->
      annotated tmp ++ " = " ++ name ++ "();"
    StReturn object ->
      "return " ++ object ++ ";"
    StNewLine ->
      ""

prettyArguments :: [String] -> String
prettyArguments arguments =
  "(" ++ intercalate ", " (map annotated arguments) ++ ")"

prettyFunction :: String -> [String] -> [Statement] -> [String]
prettyFunction name arguments statements =
  [annotated name ++ prettyArguments arguments ++ " {"]
    ++ indent (map prettyStatement statements)
    ++ ["}", ""]

prettyAbstraction :: Function -> [String]
prettyAbstraction (Function name statements) =
  prettyFunction name ["env[]", "arg"] statements

prettyDefinition :: Function -> [String]
prettyDefinition (Function name statements) =
  prettyFunction name [] statements

pretty :: Item -> [String]
pretty (Item definition abstractions) =
  concat (map prettyAbstraction abstractions) ++ prettyDefinition definition
