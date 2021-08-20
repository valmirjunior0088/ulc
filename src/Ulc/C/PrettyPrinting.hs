module Ulc.C.PrettyPrinting
  (annotated
  ,pretty
  )
  where

import Data.List (intercalate)
import Ulc.C.Generation (Statement (..), Function (..), Item (..))

annotated :: String -> String
annotated string =
  "struct object *" ++ string

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
    StIntegerSum tmp left right ->
      [annotated tmp
        ++ " = object_integer_sum("
        ++ left ++ ", " ++ right
        ++ ");"
      ]
    StReal tmp value ->
      [annotated tmp
        ++ " = object_real("
        ++ show value
        ++ ");"
      ]
    StRealSum tmp left right ->
      [annotated tmp
        ++ " = object_real_sum("
        ++ left ++ ", " ++ right
        ++ ");"
      ]
    StClosure tmp name variables ->
      if null variables
        then [header ++ footer]
        else [header ++ ","] ++ content ++ [footer]
      where
        header =
          annotated tmp
            ++ " = object_closure_" ++ show (length variables)
            ++ "(" ++ name
        content =
          indent (mapInit (++ ",") variables)
        footer =
          ");"
    StApply tmp function argument ->
      [annotated tmp
        ++ " = object_apply("
        ++ function ++ ", " ++ argument
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

pretty :: Item -> String
pretty (Item definition abstractions) =
  concat (map prettyAbstraction abstractions) ++ prettyDefinition definition
