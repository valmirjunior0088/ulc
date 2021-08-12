module Ulc
  (run
  )
  where

import Data.Function ((&))
import Data.List (intercalate)
import qualified Ulc.Core as Core
import qualified Ulc.Parsing as Parsing
import qualified Ulc.Conversion as Conversion
import qualified Ulc.Flattening as Flattening
import qualified Ulc.Generation as Generation
import qualified Ulc.PrettyPrinting as PrettyPrinting

process :: String -> Core.Term -> String
process name term =
  Conversion.convert term
    & Flattening.flatten
    & Generation.generate name
    & PrettyPrinting.pretty

header :: String
header =
  intercalate "\n"
    ["#include \"object.h\""
    ,""
    ,""
    ]
    
footer :: String
footer =
  intercalate "\n"
    ["int main(void) {"
    ,"  struct object *result = run();"
    ,"  object_debug(result);"
    ,"  object_leave(result);"
    ,""
    ,"  return 0;"
    ,"}"
    ,""
    ]

run :: String -> Either String String
run source = do
  definitions <- Parsing.runParsing Parsing.psDefinitions source
  return (header ++ concat (map (uncurry process) definitions) ++ footer)
