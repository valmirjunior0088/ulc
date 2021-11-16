module Ulc.WebAssembly.RelocBuffer
  ( RelocBuffer (..)
  , relocPrependSize
  )
  where

import Ulc.WebAssembly.Module (Offset, RelocEntry (..))
import Ulc.WebAssembly.Buffer (Buffer (..), unsigned)

data RelocBuffer =
  RelocBuffer Buffer [RelocEntry]

addOffset :: Offset -> RelocEntry -> RelocEntry
addOffset offset (RelocEntry relocType offset' symIdx) =
  RelocEntry relocType (offset + offset') symIdx

adjustRelocs :: Buffer -> [RelocEntry] -> [RelocEntry]
adjustRelocs (Buffer offset _) =
  map (addOffset offset)

instance Semigroup RelocBuffer where
  RelocBuffer buffer entries <> RelocBuffer buffer' entries' =
    RelocBuffer (buffer <> buffer') (entries ++ adjustRelocs buffer entries')

instance Monoid RelocBuffer where
  mempty = RelocBuffer mempty []
  mconcat = foldl (<>) mempty

relocPrependSize :: RelocBuffer -> RelocBuffer
relocPrependSize relocBuffer =
  let RelocBuffer (Buffer size _) _ = relocBuffer
  in RelocBuffer (unsigned size) [] <> relocBuffer 
