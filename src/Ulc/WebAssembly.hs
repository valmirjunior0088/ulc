module Ulc.WebAssembly
  ( run
  )
  where

import Ulc.Common (Item)
import Ulc.WebAssembly.Generation (generate)
import Ulc.WebAssembly.Serialization (serialize)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as ByteString

compile :: [Item] -> ByteString
compile items =
  toLazyByteString (serialize $ generate items)

run :: FilePath -> [Item] -> IO ()
run output items =
  ByteString.writeFile output (compile items)
