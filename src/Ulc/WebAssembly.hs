module Ulc.WebAssembly
  ( run
  )
  where

import Ulc.Common (Item)
import Ulc.WebAssembly.Generation (generate)
import Ulc.WebAssembly.Serialization (serialize)
import Data.ByteString.Lazy (ByteString, writeFile)
import Data.ByteString.Builder (toLazyByteString)
import Prelude hiding (writeFile)

compile :: [Item] -> ByteString
compile items =
  toLazyByteString (serialize $ generate items)

run :: FilePath -> [Item] -> IO ()
run output items =
  writeFile output (compile items)
