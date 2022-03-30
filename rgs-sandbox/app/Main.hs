module Main where

import           Criterion.Main
import qualified Data.ByteString.Lazy as BSL

import qualified Codec.CBOR.Write as CBOR (toLazyByteString)

import           Embedded.Roundtrip.EmbeddedCBOR (roundtripViaEmbeddedCBOR)
import           Embedded.Roundtrip.Normal (roundtripNormal)
import           Embedded.Roundtrip.SerialisedCBOR (roundtripViaSerialisedCBOR)
import qualified HACKY

main :: IO ()
main = defaultMain
  [ env (pure (HACKY.smallBlock, smallBSz)) $
      \ ~(smallBlock', smallBSz') ->
    bgroup "Small block"
    [ bench "encode-decode" $
        nf (toBSL . roundtripNormal smallBlock' . HACKY.deserialiseB) smallBlock'
    , bench "encode-encodeBytes-decodeBytes-decode" $
        nf (toBSL . roundtripViaSerialisedCBOR smallBlock' . HACKY.deserialiseB) smallBlock'
    , bench "encode-encodeEmbeddedCBOR-decodeEmbeddedCBOR-decode" $
        nf (toBSL . roundtripViaEmbeddedCBOR smallBSz' smallBlock' . HACKY.deserialiseB) smallBlock'
    ]
  , env (pure (HACKY.bigBlock, bigBSz)) $
      \ ~(bigBlock', bigBSz') ->
    bgroup "Big block"
    [ bench "encode-decode" $
        nf (toBSL . roundtripNormal bigBlock' . HACKY.deserialiseB) bigBlock'
    , bench "encode-encodeBytes-decodeBytes-decode" $
        nf (toBSL . roundtripViaSerialisedCBOR bigBlock' . HACKY.deserialiseB) bigBlock'
    , bench "encode-encodeEmbeddedCBOR-decodeEmbeddedCBOR-decode" $
        nf (toBSL . roundtripViaEmbeddedCBOR bigBSz' bigBlock' . HACKY.deserialiseB) bigBlock'
    ]
  ]
  where
    smallBSz :: Word
    smallBSz = fromIntegral $ BSL.length HACKY.smallBlock

    bigBSz :: Word
    bigBSz = fromIntegral $ BSL.length HACKY.bigBlock

    -- B doesn't have an NFData instance, but ByteString does
    toBSL :: HACKY.B -> BSL.ByteString
    toBSL = CBOR.toLazyByteString . HACKY.encodeB
