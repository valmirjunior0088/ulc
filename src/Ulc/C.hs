module Ulc.C
  (run
  )
  where

import Ulc.Common (Definition (..), Item (..), prepare)
import Ulc.C.Generation (generate)
import Ulc.C.PrettyPrinting (annotated, pretty)

header :: [String]
header =
  ["#include \"object.h\""
  ,""
  ]

footer :: [String]
footer =
  ["int main(void) {"
  ,"  struct object *result = run();"
  ,"  object_debug(result);"
  ,"  object_leave(result);"
  ,""
  ,"  return 0;"
  ,"}"
  ]

declare :: Item -> String
declare (Item (Definition name _) _) =
  annotated name ++ "();"

process :: [Item] -> [String]
process items =
  map declare items ++ [""] ++ concat (map (pretty . generate) items)

run :: String -> Either String String
run source = do
  items <- prepare source
  return (unlines $ header ++ process items ++ footer)
