module Ulc
  (run
  )
  where

import qualified Ulc.C as C
import qualified Ulc.WebAssembly as WebAssembly

run :: String -> String -> Either String String
run mode source =
  case mode of
    "c" -> C.run source
    "wasm" -> WebAssembly.run source
    _ -> Left ("Unknown mode: " ++ mode)
