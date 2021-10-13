module Ulc.C
  ( run
  )
  where

import Ulc.Common (Definition (..), Item (..))
import Ulc.C.Generation (generate)
import Ulc.C.PrettyPrinting (annotated, pretty)

header :: [String]
header =
  [ "#include \"object.h\""
  , ""
  ]

footer :: [String]
footer =
  [ "int main() {"
  , "  struct object *result = main$def();"
  , "  object_debug(result);"
  , "  object_leave(result);"
  , ""
  , "  return 0;"
  , "}"
  ]

declare :: Item -> String
declare (Item (Definition name _) _) =
  annotated name ++ "();"

process :: [Item] -> [String]
process items =
  map declare items ++ [""] ++ concat (map (pretty . generate) items)

compile :: [Item] -> String
compile items =
  unlines (header ++ process items ++ footer)

run :: FilePath -> [Item] -> IO ()
run output items =
  writeFile output (compile items)
