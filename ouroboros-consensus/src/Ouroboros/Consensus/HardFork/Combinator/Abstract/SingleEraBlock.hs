{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE KindSignatures #-}

module Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock (
    -- * Single era block
    SingleEraBlock (..)
  , proxySingle
  , singleEraTransition'
    -- * Era index
  , EraIndex (..)
  , eraIndexEmpty
  , eraIndexFromIndex
  , eraIndexFromNS
  , eraIndexSucc
  , eraIndexToInt
  , eraIndexZero
  ) where

import           Codec.Serialise
import           Data.Either (isRight)
import           Data.Proxy
import           Data.SOP.Strict
import qualified Data.Text as Text
import           Data.Void

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config.SupportsNode
import           Ouroboros.Consensus.HardFork.History (Bound, EraParams)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.CommonProtocolParams
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Ledger.Query
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Ledger.SupportsPeerSelection
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Node.InitStorage
import           Ouroboros.Consensus.Storage.Serialisation
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.SOP

import           Ouroboros.Consensus.HardFork.Combinator.Info
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Ouroboros.Consensus.HardFork.Combinator.Util.Match

{-------------------------------------------------------------------------------
  SingleEraBlock
-------------------------------------------------------------------------------}

-- | Blocks from which we can assemble a hard fork
class ( LedgerSupportsProtocol i blk
      , InspectLedger blk
      , LedgerSupportsMempool i blk
      , HasTxId (GenTx blk)
      , QueryLedger blk
      , HasPartialConsensusConfig (BlockProtocol blk)
      , HasPartialLedgerConfig i blk
      , ConvertRawHash blk
      , ReconstructNestedCtxt Header blk
      , CommonProtocolParams i blk
      , LedgerSupportsPeerSelection blk
      , ConfigSupportsNode blk
      , NodeInitStorage i blk
      , BlockSupportsMetrics blk
        -- Instances required to support testing
      , Eq   (GenTx blk)
      , Eq   (Validated (GenTx blk))
      , Eq   (ApplyTxErr blk)
      , Show blk
      , Show (Header blk)
      , Show (CannotForge blk)
      , Show (ForgeStateInfo blk)
      , Show (ForgeStateUpdateError blk)
      ) => SingleEraBlock i blk where

  -- | Era transition
  --
  -- This should only report the transition point once it is stable (rollback
  -- cannot affect it anymore).
  --
  -- Since we need this to construct the 'HardForkSummary' (and hence the
  -- 'EpochInfo'), this takes the /partial/ config, not the full config
  -- (or we'd end up with a catch-22).
  singleEraTransition :: PartialLedgerConfig i blk
                      -> EraParams -- ^ Current era parameters
                      -> Bound     -- ^ Start of this era
                      -> LedgerState i blk mk
                      -> Maybe EpochNo

  -- | Era information (for use in error messages)
  singleEraInfo       :: proxy blk -> SingleEraInfo i blk

proxySingle :: Proxy (SingleEraBlock i)
proxySingle = Proxy

singleEraTransition' :: SingleEraBlock i blk
                     => WrapPartialLedgerConfig i blk
                     -> EraParams
                     -> Bound
                     -> LedgerState i blk mk -> Maybe EpochNo
singleEraTransition' = singleEraTransition . unwrapPartialLedgerConfig

{-------------------------------------------------------------------------------
  Era index
-------------------------------------------------------------------------------}

newtype EraIndex (i :: Implementation) xs = EraIndex {
      getEraIndex :: NS (K ()) xs
    }

instance Eq (EraIndex i xs) where
  EraIndex era == EraIndex era' = isRight (matchNS era era')

instance All (SingleEraBlock i) xs => Show (EraIndex i xs) where
  show = hcollapse . hcmap proxySingle (getEraName @i) . getEraIndex
    where
      getEraName :: forall blk. SingleEraBlock i blk
                 => K () blk -> K String blk
      getEraName _ =
            K
          . (\name -> "<EraIndex " <> name <> ">")
          . Text.unpack
          . singleEraName
          $ singleEraInfo @i (Proxy @blk)

instance All (SingleEraBlock i) xs => Condense (EraIndex i xs) where
  condense = hcollapse . hcmap proxySingle getEraName . getEraIndex
    where
      getEraName :: forall blk. SingleEraBlock i blk
                 => K () blk -> K String blk
      getEraName _ =
            K
          . Text.unpack
          . singleEraName
          $ singleEraInfo @i (Proxy @blk)

instance SListI xs => Serialise (EraIndex i xs) where
  encode = encode . nsToIndex . getEraIndex
  decode = do
    idx <- decode
    case nsFromIndex idx of
      Nothing       -> fail $ "EraIndex: invalid index " <> show idx
      Just eraIndex -> return (EraIndex eraIndex)

eraIndexEmpty :: EraIndex i '[] -> Void
eraIndexEmpty (EraIndex ns) = case ns of {}

eraIndexFromNS :: SListI xs => NS f xs -> EraIndex i xs
eraIndexFromNS = EraIndex . hmap (const (K ()))

eraIndexFromIndex :: Index xs blk -> EraIndex i xs
eraIndexFromIndex index = EraIndex $ injectNS index (K ())

eraIndexZero :: EraIndex i (x ': xs)
eraIndexZero = EraIndex (Z (K ()))

eraIndexSucc :: EraIndex i xs -> EraIndex i (x ': xs)
eraIndexSucc (EraIndex ix) = EraIndex (S ix)

eraIndexToInt :: EraIndex i xs -> Int
eraIndexToInt = index_NS . getEraIndex
