module Ulc.PrettyPrinting
  (pretty
  )
  where

import Data.List (intercalate)
import Ulc.Generation (Statement (..), Function (..))

annotation :: String
annotation =
  "struct object *"

annotated :: String -> String
annotated string =
  annotation ++ string

indent :: [String] -> [String]
indent =
  map ("  " ++)

mapInit :: (a -> a) -> [a] -> [a]
mapInit f list =
  case list of
    [] -> []
    x : [] -> x : []
    x : xs -> f x : mapInit f xs

prettyStatement :: Statement -> [String]
prettyStatement statement =
  case statement of
    StEnter object ->
      ["object_enter(" ++ object ++ ");"]
    StLeave object ->
      ["object_leave(" ++ object ++ ");"]
    StInteger tmp value ->
      [annotated tmp
        ++ " = object_integer("
        ++ show value
        ++ ");"
      ]
    StReal tmp value ->
      [annotated tmp
        ++ " = object_real("
        ++ show value
        ++ ");"
      ]
    StClosure tmp name variables ->
      if null variables
        then [prefix ++ suffix]
        else [prefix] ++ content ++ [suffix]
      where
        prefix =
          annotated tmp
            ++ " = object_closure("
            ++ name ++ ", "
            ++ show (length variables) ++ ", "
            ++ "(" ++ annotation ++ "[]) {"
        content =
          indent (mapInit (++ ",") variables)
        suffix =
          "});"
    StApply tmp function argument ->
      [annotated tmp
        ++ " = object_apply("
        ++ function ++ ", " ++ argument
        ++ ");"
      ]
    StIntegerSum tmp left right ->
      [annotated tmp
        ++ " = object_integer_sum("
        ++ left ++ ", " ++ right
        ++ ");"
      ]
    StRealSum tmp left right ->
      [annotated tmp
        ++ " = object_real_sum("
        ++ left ++ ", " ++ right
        ++ ");"
      ]
    StCall tmp reference ->
      [annotated tmp ++ " = " ++ reference ++ "();"]
    StReturn object ->
      ["return " ++ object ++ ";"]
    StNewLine ->
      [""]

prettyArguments :: [String] -> String
prettyArguments arguments =
  "(" ++ intercalate ", " (map annotated arguments) ++ ")"

prettyStatements :: [Statement] -> String
prettyStatements statements =
  intercalate "\n" (indent $ concat $ map prettyStatement statements)

prettyFunction :: String -> [String] -> [Statement] -> String
prettyFunction name arguments statements =
  unlines
    [annotated name ++ prettyArguments arguments ++ " {"
    ,prettyStatements statements
    ,"}"
    ,""
    ]

prettyAbstraction :: Function -> String
prettyAbstraction (Function name statements) =
  prettyFunction name ["env[]", "arg"] statements

prettyDefinition :: Function -> String
prettyDefinition (Function name statements) =
  prettyFunction name [] statements

pretty :: (Function, [Function]) -> String
pretty (definition, abstractions) =
  concat (map prettyAbstraction abstractions)
    ++ prettyDefinition definition
