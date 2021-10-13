module Ulc
  ( run
  )
  where

import qualified Ulc.Common as Common
import qualified Ulc.C as C
import qualified Ulc.WebAssembly as WebAssembly
import System.Exit (die)

run :: String -> FilePath -> FilePath -> IO ()
run mode input output = do
  result <- Common.prepare <$> readFile input

  items <- case result of
    Left message -> die message
    Right items -> return items

  case mode of
    "c" -> C.run output items
    "wasm" -> WebAssembly.run output items
    _ -> die ("Unknown mode: " ++ mode)
