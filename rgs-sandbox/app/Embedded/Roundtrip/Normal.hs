{-# LANGUAGE RankNTypes #-}
module Embedded.Roundtrip.Normal
  ( roundtripNormal
  ) where

import           Control.Exception (throw)
import qualified Data.ByteString.Lazy as BSL

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Read (deserialiseFromBytes)
import           Codec.CBOR.Write (toLazyByteString)

import qualified HACKY

-- | A typical roundtripping through CBOR.
roundtripNormal :: BSL.ByteString -> HACKY.B -> HACKY.B
roundtripNormal bytes t
  = deserialiseWith (HACKY.decodeB bytes)
  $ toLazyByteString
  $ HACKY.encodeB t

deserialiseWith :: (forall s. Decoder s a) -> BSL.ByteString -> a
deserialiseWith dec bytes =
  case deserialiseFromBytes dec bytes of
    Left failure        -> throw failure
    Right (trailing, _)  | not (BSL.null trailing)
                        -> error "Embedded.Roundtrip.Normal.deserialiseWith: trailing data"
    Right (_, t)        -> t
