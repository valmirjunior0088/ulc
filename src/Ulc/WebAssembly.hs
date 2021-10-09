module Ulc.WebAssembly
  (run
  )
  where

import Ulc.Common (Item)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString

compile :: [Item] -> ByteString
compile _ =
  undefined

run :: FilePath -> [Item] -> IO ()
run output items =
  ByteString.writeFile output (compile items)
