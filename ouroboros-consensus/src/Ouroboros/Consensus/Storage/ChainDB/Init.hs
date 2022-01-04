{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Intended for qualified import
--
-- > import Ouroboros.Consensus.Storage.ChainDB.Init (InitChainDB)
-- > import qualified Ouroboros.Consensus.Storage.ChainDB.Init as InitChainDB
module Ouroboros.Consensus.Storage.ChainDB.Init (
    InitChainDB (..)
  , fromFull
  , map
  ) where

import           Prelude hiding (map)

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import           Ouroboros.Consensus.Util.IOLike

-- | Restricted interface to the 'ChainDB' used on node initialization
data InitChainDB m i blk = InitChainDB {
      -- | Add a block to the DB
      addBlock         :: blk -> m ()

      -- | Return the current ledger state
    , getCurrentLedger :: m (LedgerState i blk EmptyMK)
    }

fromFull ::
     (IsLedger (LedgerState i blk), IOLike m)
  => ChainDB m i blk -> InitChainDB m i blk
fromFull db = InitChainDB {
      addBlock         = ChainDB.addBlock_ db
    , getCurrentLedger =
        atomically $ ledgerState <$> ChainDB.getCurrentLedger db
    }

map ::
     Functor m
  => (blk' -> blk)
  -> (LedgerState i blk EmptyMK -> LedgerState i blk' EmptyMK)
  -> InitChainDB m i blk -> InitChainDB m i blk'
map f g db = InitChainDB {
      addBlock         = addBlock db . f
    , getCurrentLedger = g <$> getCurrentLedger db
    }
