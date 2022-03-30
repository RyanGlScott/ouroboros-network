{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Embedded.Roundtrip.SerialisedCBOR
  ( roundtripViaSerialisedCBOR
  ) where

import           Control.Exception (throw)
import qualified Data.ByteString.Lazy as BSL

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Read (deserialiseFromBytes)
import           Codec.CBOR.Write (toLazyByteString)
import           Codec.Serialise (Serialise(..))

import qualified HACKY

-- | A roundtripping that also serialises the 'BSL.ByteString' representing the
-- serialised value.
roundtripViaSerialisedCBOR :: BSL.ByteString -> HACKY.B -> HACKY.B
roundtripViaSerialisedCBOR bytes t
  = deserialiseWith (HACKY.decodeB bytes)
  $ deserialiseWith (decode @BSL.ByteString)
  $ toLazyByteString
  $ encode @BSL.ByteString
  $ toLazyByteString
  $ HACKY.encodeB t

deserialiseWith :: (forall s. Decoder s a) -> BSL.ByteString -> a
deserialiseWith dec bytes =
  case deserialiseFromBytes dec bytes of
    Left failure        -> throw failure
    Right (trailing, _)  | not (BSL.null trailing)
                        -> error "Embedded.Roundtrip.SerialisedCBOR.deserialiseWith: trailing data"
    Right (_, t)        -> t
