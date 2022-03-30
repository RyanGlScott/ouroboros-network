{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Embedded.Roundtrip.EmbeddedCBOR
  ( roundtripViaEmbeddedCBOR
  ) where

import           Control.Exception (throw)
import qualified Data.ByteString.Lazy as BSL

import           Codec.CBOR.Decoding (Decoder, decodeEmbeddedCBOR)
import           Codec.CBOR.Encoding (encodeEmbeddedCBOR)
import           Codec.CBOR.Read (deserialiseFromBytes)
import           Codec.CBOR.Write (toLazyByteString)

import qualified HACKY

-- | A roundtripping that embeds the CBOR of the serialised value.
roundtripViaEmbeddedCBOR :: Word -- ^ Invariant: equal to the 'BSL.length' of the second argument
                         -> BSL.ByteString
                         -> HACKY.B -> HACKY.B
roundtripViaEmbeddedCBOR sz bytes t
  = deserialiseWith (decodeEmbeddedCBOR (HACKY.decodeB bytes))
  $ toLazyByteString
  $ encodeEmbeddedCBOR sz
  $ HACKY.encodeB t

deserialiseWith :: (forall s. Decoder s a) -> BSL.ByteString -> a
deserialiseWith dec bytes =
  case deserialiseFromBytes dec bytes of
    Left failure        -> throw failure
    Right (trailing, _)  | not (BSL.null trailing)
                        -> error "Embedded.Roundtrip.EmbeddedCBOR.deserialiseWith: trailing data"
    Right (_, t)        -> t
