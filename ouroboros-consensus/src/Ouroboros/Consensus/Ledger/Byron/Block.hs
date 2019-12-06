{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Ouroboros.Consensus.Ledger.Byron.Block (
    -- * Hash
    ByronHash(..)
  , mkByronHash
  , byronHashInfo
    -- * Block
  , ByronBlock(..)
  , mkByronBlock
  , annotateByronBlock
    -- * Header
  , Header(..)
  , mkByronHeader
  , byronBlockMatchesHeader
    -- * Serialisation
  , encodeByronBlockWithInfo
  , encodeByronBlock
  , decodeByronBlock
  , encodeByronHeader
  , decodeByronHeader
  , encodeByronHeaderHash
  , decodeByronHeaderHash
  ) where

import           Data.Binary (Get, Put)
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import qualified Data.ByteArray as ByteArray
import           Data.ByteString (ByteString)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import           Data.FingerTree.Strict (Measured (..))
import           Data.Proxy
import           Data.Typeable
import           Data.Word (Word32)
import           GHC.Generics (Generic)

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR

import           Cardano.Binary
import           Cardano.Prelude (NoUnexpectedThunks (..))

import qualified Crypto.Hash as Crypto

import qualified Cardano.Chain.Block as CC
import           Cardano.Chain.Byron.Auxiliary
import qualified Cardano.Chain.Slotting as CC
import qualified Cardano.Crypto.Hashing as CC

import           Ouroboros.Network.Block (StandardHash, HasHeader(..),
                     HeaderHash, ChainHash, BlockMeasure, SlotNo, castHash,
                     blockMeasure)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Byron.Conversions
import           Ouroboros.Consensus.Ledger.Byron.Orphans ()
import           Ouroboros.Consensus.Util.Condense

import           Ouroboros.Storage.ImmutableDB (BinaryInfo (..), HashInfo (..))

{-------------------------------------------------------------------------------
  Header hash
-------------------------------------------------------------------------------}

newtype ByronHash = ByronHash { unByronHash :: CC.HeaderHash }
  deriving stock   (Eq, Ord, Show, Generic)
  deriving newtype (ToCBOR, FromCBOR, Condense)
  deriving anyclass NoUnexpectedThunks

mkByronHash :: BlockOrBoundaryHdr -> ByronHash
mkByronHash = undefined -- ByronHash . abobHdrHash

byronHashInfo :: HashInfo ByronHash
byronHashInfo = HashInfo { hashSize, getHash, putHash }
  where
    hashSize :: Word32
    hashSize = fromIntegral $ CC.hashDigestSize' @Crypto.Blake2b_256

    getHash :: Get ByronHash
    getHash = do
      bytes <- Get.getByteString (fromIntegral hashSize)
      case Crypto.digestFromByteString bytes of
        Nothing     -> fail "digestFromByteString failed"
        Just digest -> return $ ByronHash $ CC.AbstractHash digest

    putHash :: ByronHash -> Put
    putHash (ByronHash (CC.AbstractHash digest)) =
      Put.putByteString $ ByteArray.convert digest

{-------------------------------------------------------------------------------
  Block
-------------------------------------------------------------------------------}

-- | Byron block
--
-- We cache two bits of information:
--
-- * We cache the slot number as this is not readily available for EBBs.
--   Having it cached allows us to e.g. give a 'HasHeader' instance.
-- * We cache the hash as this is expensive to compute and we need it often.
data ByronBlock = ByronBlock {
      byronBlockRaw    :: !CC.BlockOrBoundary
    , byronBlockSlotNo :: !SlotNo
    , byronBlockHash   :: !ByronHash
    }
  deriving (Eq, Show)

instance Condense ByronBlock where
  condense = condense . byronBlockRaw

mkByronBlock :: CC.EpochSlots -> CC.BlockOrBoundary -> ByronBlock
mkByronBlock epochSlots blk = ByronBlock {
      byronBlockRaw    = blk
    , byronBlockSlotNo = undefined -- fromByronSlotNo $ abobHdrSlotNo epochSlots hdr
    , byronBlockHash   = mkByronHash hdr
    }
  where
    hdr = undefined
    -- hdr = abobHdrFromBlock blk

-- | Construct Byron block from unannotated 'CC.Block'
--
-- This should be used only when forging blocks (not when receiving blocks
-- over the wire).
annotateByronBlock :: CC.EpochSlots -> CC.Block -> ByronBlock
annotateByronBlock es = undefined -- mkByronBlock es . CC.BOBBlock . reAnnotateBlock es

{-------------------------------------------------------------------------------
  Header
-------------------------------------------------------------------------------}

instance GetHeader ByronBlock where
  -- | Byron header
  --
  -- See 'ByronBlock' for comments on why we cache certain values.
  data Header ByronBlock = ByronHeader {
        byronHeaderRaw    :: !BlockOrBoundaryHdr
      , byronHeaderSlotNo :: !SlotNo
      , byronHeaderHash   :: !ByronHash
      }
    deriving (Eq, Show, Generic)

  getHeader ByronBlock{..} = ByronHeader{
        byronHeaderRaw    = undefined -- abobHdrFromBlock byronBlockRaw
      , byronHeaderSlotNo = byronBlockSlotNo
      , byronHeaderHash   = byronBlockHash
      }

instance Condense (Header ByronBlock) where
  condense = undefined -- aBlockOrBoundaryHdr condense condense . byronHeaderRaw

instance NoUnexpectedThunks (Header ByronBlock) where
  showTypeOf _ = show $ typeRep (Proxy @(Header ByronBlock))

mkByronHeader :: CC.EpochSlots
              -> BlockOrBoundaryHdr
              -> Header ByronBlock
mkByronHeader epochSlots hdr = ByronHeader {
      byronHeaderRaw    = hdr
    , byronHeaderSlotNo = undefined -- fromByronSlotNo $ abobHdrSlotNo epochSlots hdr
    , byronHeaderHash   = mkByronHash hdr
    }

-- | Check if a block matches its header
byronBlockMatchesHeader :: Header ByronBlock -> ByronBlock -> Bool
byronBlockMatchesHeader hdr blk = undefined
    -- abobMatchesBody (byronHeaderRaw hdr) (byronBlockRaw blk)

{-------------------------------------------------------------------------------
  HasHeader instances

  This doesn't do much more than pass to the instance for headers.
-------------------------------------------------------------------------------}

type instance HeaderHash ByronBlock = ByronHash
instance StandardHash ByronBlock

instance HasHeader ByronBlock where
  blockHash      =            blockHash     . getHeader
  blockPrevHash  = castHash . blockPrevHash . getHeader
  blockSlot      =            blockSlot     . getHeader
  blockNo        =            blockNo       . getHeader
  blockInvariant = const True

instance HasHeader (Header ByronBlock) where
  blockHash      = byronHeaderHash
  blockSlot      = byronHeaderSlotNo
  blockPrevHash  = undefined -- fromByronPrevHash' . abobHdrPrevHash        . byronHeaderRaw
  blockNo        = undefined -- fromByronBlockNo   . abobHdrChainDifficulty . byronHeaderRaw
  blockInvariant = const True

instance Measured BlockMeasure ByronBlock where
  measure = blockMeasure

fromByronPrevHash' :: Maybe CC.HeaderHash -> ChainHash (Header ByronBlock)
fromByronPrevHash' = fromByronPrevHash ByronHash

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

encodeByronHeaderHash :: HeaderHash ByronBlock -> Encoding
encodeByronHeaderHash = toCBOR

decodeByronHeaderHash :: Decoder s (HeaderHash ByronBlock)
decodeByronHeaderHash = fromCBOR

-- | 'encodeByronBlock' including the offset and size of the header within the
-- resulting bytestring.
--
-- NOTE: the bytestring obtained by slicing the serialised block using the
-- header offset and size will correspond to the /header annotation/, but not
-- to the serialised header, as we add an envelope ('encodeListLen' + tag)
-- around a header in 'encodeByronHeader'. This envelope must thus still be
-- added to the sliced bytestring before it can be deserialised using
-- 'decodeByronHeader'.
encodeByronBlockWithInfo :: ByronBlock -> BinaryInfo Encoding
encodeByronBlockWithInfo blk = undefined {- BinaryInfo
    { binaryBlob   = encodeByronBlock blk
    , headerOffset = 1 {- 'encodeListLen' of the outer 'Either' envelope -}
                   + 1 {- the tag -}
                   + 1 {- 'encodeListLen' of the block: header + body + ...  -}
      -- Compute the length of the annotated header
    , headerSize   = fromIntegral $ Strict.length $ case byronBlockRaw blk of
        CC.BOBBoundary b -> CC.boundaryHeaderAnnotation $ CC.boundaryHeader b
        CC.BOBBlock    b -> CC.headerAnnotation         $ CC.blockHeader    b
    }
-}

-- | Encode a block
--
-- Should be backwards compatible with legacy (cardano-sl) nodes.
--
-- Implementation note: the decoder uses 'CC.fromCBORABlockOrBoundary', which
-- has inverse 'CC.toCBORABlockOrBoundary'. This encoder is intended to be
-- binary compatible with 'CC.toCBORABlockOrBoundary', but does not use it and
-- instead takes advantage of the annotations (using 'encodePreEncoded').
encodeByronBlock :: ByronBlock -> Encoding
encodeByronBlock blk = undefined {- mconcat [
      CBOR.encodeListLen 2
    , case byronBlockRaw blk of
        CC.BOBBoundary b -> mconcat [
            CBOR.encodeWord 0
          , CBOR.encodePreEncoded $ CC.boundaryAnnotation b
          ]
        CC.BOBBlock b -> mconcat [
            CBOR.encodeWord 1
          , CBOR.encodePreEncoded $ CC.blockAnnotation b
          ]
    ]
-}

-- | Inverse of 'encodeByronBlock'
decodeByronBlock :: CC.EpochSlots -> Decoder s (Lazy.ByteString -> ByronBlock)
decodeByronBlock epochSlots = undefined {-
    fillInByteString <$> CC.fromCBORABlockOrBoundary epochSlots
  where
    fillInByteString :: CC.ABlockOrBoundary ByteSpan
                     -> Lazy.ByteString
                     -> ByronBlock
    fillInByteString it theBytes = mkByronBlock epochSlots $
      Lazy.toStrict . slice theBytes <$> it
-}

-- | Encode a header
--
-- Should be backwards compatible with legacy (cardano-sl) nodes.
--
-- This function should be inverse to 'decodeByronHeader'
-- (which uses 'fromCBORABlockOrBoundaryHdr').
encodeByronHeader :: Header ByronBlock -> Encoding
encodeByronHeader hdr = undefined {- mconcat [
      CBOR.encodeListLen 2
    , case byronHeaderRaw hdr of
        ABOBBoundaryHdr h -> mconcat [
            CBOR.encodeWord 0
          , CBOR.encodePreEncoded $ CC.boundaryHeaderAnnotation h
          ]
        ABOBBlockHdr h -> mconcat [
            CBOR.encodeWord 1
          , CBOR.encodePreEncoded $ CC.headerAnnotation h
          ]
    ]
-}

-- | Inverse of 'encodeByronHeader'
decodeByronHeader :: CC.EpochSlots
                  -> Decoder s (Lazy.ByteString -> Header ByronBlock)
decodeByronHeader epochSlots = undefined {-
    fillInByteString <$> fromCBORABlockOrBoundaryHdr epochSlots
  where
    fillInByteString :: ABlockOrBoundaryHdr ByteSpan
                     -> Lazy.ByteString
                     -> Header ByronBlock
    fillInByteString it theBytes = mkByronHeader epochSlots $
      Lazy.toStrict . slice theBytes <$> it
-}
