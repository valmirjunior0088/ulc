module Ulc.WebAssembly.Serialization
  ( serialize
  )
  where

import Ulc.WebAssembly.Generation (Module (..))
import Data.ByteString.Lazy (ByteString)

serialize :: Module -> ByteString
serialize _ =
  undefined
