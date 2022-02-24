{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test.Ouroboros.Network.Testnet (tests) where

import           Control.Monad.IOSim
import           Control.Monad.Class.MonadTime (Time (Time), DiffTime, diffTime)
import           Control.Tracer (Tracer (Tracer), contramap, nullTracer)

import           Data.Bifoldable (bifoldMap)
import           Data.Void (Void)
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Monoid (Sum(Sum))
import           Data.Dynamic (Typeable)
import           Data.Functor (void)
import           Data.List (intercalate)
import qualified Data.List.Trace as Trace

import           System.Random (mkStdGen)
import           GHC.Exception.Type (SomeException)

import           Ouroboros.Network.Testing.Data.AbsBearerInfo
                     (AbsBearerInfo (..), attenuation, delay, toSduSize)
import           Ouroboros.Network.PeerSelection.Governor
                      (TracePeerSelection (..), DebugPeerSelection (..))
import           Ouroboros.Network.Testing.Data.Signal
                      (Events, Signal, eventsToList,
                      signalProperty)
import           Ouroboros.Network.PeerSelection.RootPeersDNS
                      (TraceLocalRootPeers, TracePublicRootPeers)
import           Ouroboros.Network.PeerSelection.Types (PeerStatus(..))
import           Ouroboros.Network.Driver.Limits (ProtocolTimeLimits(..))
import           Ouroboros.Network.Diffusion.P2P
                      (TracersExtra(..), RemoteTransitionTrace)
import           Ouroboros.Network.ConnectionHandler (ConnectionHandlerTrace)
import           Ouroboros.Network.ConnectionManager.Types
import qualified Ouroboros.Network.Testing.Data.Signal as Signal
import qualified Ouroboros.Network.PeerSelection.Governor as Governor
import qualified Ouroboros.Network.PeerSelection.LocalRootPeers as LocalRootPeers
import qualified Ouroboros.Network.PeerSelection.EstablishedPeers as EstablishedPeers
import           Ouroboros.Network.Protocol.Handshake.Type
                      (ClientHasAgency(..), ServerHasAgency (..))
import qualified Ouroboros.Network.Diffusion.P2P as Diff.P2P
import           Ouroboros.Network.Testing.Utils
                      (WithTime(..), WithName(..), tracerWithTime,
                      tracerWithName, sayTracer, splitWithNameTrace)
import           Ouroboros.Network.Protocol.Handshake.Codec (timeLimitsHandshake)

import           Network.TypedProtocol (PeerHasAgency(..))

import           Simulation.Network.Snocket (BearerInfo (..))

import           Test.Ouroboros.Network.Testnet.Simulation.Node
                     (DiffusionScript (..), diffusionSimulation,
                     prop_diffusionScript_commandScript_valid,
                     prop_diffusionScript_fixupCommands,
                     DiffusionSimulationTrace (..))
import           Test.Ouroboros.Network.Testnet.Simulation.ConnectionManager
                     (TestProperty(..), mkProperty, ppTransition,
                     AllProperty (..), classifyPrunings,
                     classifyNegotiatedDataFlow, classifyEffectiveDataFlow,
                     verifyAbstractTransition, classifyActivityType,
                     classifyTermination, groupConns,
                     verifyAbstractTransitionOrder)
import           Test.Ouroboros.Network.Testnet.Simulation.InboundGovernor
                     (verifyRemoteTransition, splitRemoteConns,
                     verifyRemoteTransitionOrder)
import           Test.Ouroboros.Network.Diffusion.Node.NodeKernel
import           Test.QuickCheck (Property, counterexample, conjoin, property, (.&&.))
import           Test.Tasty
import           Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Testnet"
  [ testGroup "multinodeSim"
    [ testProperty "diffusionScript fixupCommands idempotent"
                   prop_diffusionScript_fixupCommands
    , testProperty "diffusionScript command script valid"
                   prop_diffusionScript_commandScript_valid
    , testProperty "diffusion target established local"
                   prop_diffusion_target_established_local
    , testProperty "diffusion target active below"
                   prop_diffusion_target_active_below
    , testProperty "diffusion target active local above"
                   prop_diffusion_target_active_local_above
    , testProperty "diffusion connection manager valid transitions"
                   prop_diffusion_cm_valid_transitions
    , testProperty "diffusion connection manager valid transition order"
                   prop_diffusion_cm_valid_transition_order
    , testProperty "diffusion inbound governor valid transitions"
                   prop_diffusion_ig_valid_transitions
    , testProperty "diffusion inbound governor valid transition order"
                   prop_diffusion_ig_valid_transition_order
    , testProperty "diffusion cm & ig timeouts enforced"
                   prop_diffusion_timeouts_enforced
    ]
  ]


-- Warning: be careful with writing properties that rely
-- on trace events from multiple components environment.
-- These events typically occur in separate threads and
-- so are not casually ordered. It is ok to use them for
-- timeout/eventually properties, but not for properties
-- that check conditions synchronously.
--
data DiffusionTestTrace =
      DiffusionLocalRootPeerTrace (TraceLocalRootPeers NtNAddr SomeException)
    | DiffusionPublicRootPeerTrace TracePublicRootPeers
    | DiffusionPeerSelectionTrace (TracePeerSelection NtNAddr)
    | DiffusionDebugPeerSelectionTrace (DebugPeerSelection NtNAddr ())
    | DiffusionConnectionManagerTrace
        (ConnectionManagerTrace NtNAddr
          (ConnectionHandlerTrace NtNVersion NtNVersionData))
    | DiffusionDiffusionSimulationTrace DiffusionSimulationTrace
    | DiffusionConnectionManagerTransitionTrace
        (AbstractTransitionTrace NtNAddr)
    | DiffusionInboundGovernorTransitionTrace
        (RemoteTransitionTrace NtNAddr)
    deriving (Show)

tracersExtraWithTimeName
  :: NtNAddr
  -> Diff.P2P.TracersExtra NtNAddr NtNVersion NtNVersionData
                           NtCAddr NtCVersion NtCVersionData
                           SomeException (IOSim s)
tracersExtraWithTimeName ntnAddr =
  Diff.P2P.TracersExtra {
    dtTraceLocalRootPeersTracer           = contramap
                                             DiffusionLocalRootPeerTrace
                                          . tracerWithName ntnAddr
                                          . tracerWithTime
                                          $ dynamicTracer
    , dtTracePublicRootPeersTracer        = contramap
                                             DiffusionPublicRootPeerTrace
                                          . tracerWithName ntnAddr
                                          . tracerWithTime
                                          $ dynamicTracer
    , dtTracePeerSelectionTracer          = contramap
                                             DiffusionPeerSelectionTrace
                                          . tracerWithName ntnAddr
                                          . tracerWithTime
                                          $ dynamicTracer
    , dtDebugPeerSelectionInitiatorTracer = contramap
                                             ( DiffusionDebugPeerSelectionTrace
                                             . void
                                             )
                                          . tracerWithName ntnAddr
                                          . tracerWithTime
                                          $ dynamicTracer
    , dtDebugPeerSelectionInitiatorResponderTracer
        = contramap
           ( DiffusionDebugPeerSelectionTrace
           . void
           )
        . tracerWithName ntnAddr
        . tracerWithTime
        $ dynamicTracer
    , dtTracePeerSelectionCounters        = nullTracer
    , dtPeerSelectionActionsTracer        = nullTracer
    , dtConnectionManagerTracer           = contramap
                                             DiffusionConnectionManagerTrace
                                          . tracerWithName ntnAddr
                                          . tracerWithTime
                                          $ dynamicTracer
    , dtConnectionManagerTransitionTracer = contramap
                                              DiffusionConnectionManagerTransitionTrace
                                          . tracerWithName ntnAddr
                                          . tracerWithTime
                                          $ dynamicTracer
    , dtServerTracer                      = nullTracer
    , dtInboundGovernorTracer             = nullTracer
    , dtInboundGovernorTransitionTracer   = contramap
                                              DiffusionInboundGovernorTransitionTrace
                                          . tracerWithName ntnAddr
                                          . tracerWithTime
                                          $ dynamicTracer
    , dtLocalConnectionManagerTracer      = nullTracer
    , dtLocalServerTracer                 = nullTracer
    , dtLocalInboundGovernorTracer        = nullTracer
  }

tracerDiffusionSimWithTimeName :: NtNAddr -> Tracer (IOSim s) DiffusionSimulationTrace
tracerDiffusionSimWithTimeName ntnAddr =
   contramap DiffusionDiffusionSimulationTrace
 . tracerWithName ntnAddr
 . tracerWithTime
 $ dynamicTracer


-- | A variant of
-- 'Test.Ouroboros.Network.PeerSelection.prop_governor_target_established_local'
-- but for running on Diffusion. This means it has to have in consideration the
-- the logs for all nodes running will all appear in the trace and the test
-- property should only be valid while a given node is up and running.
--
-- We do not need separate above and below variants of this property since it
-- is not possible to exceed the target.
--
prop_diffusion_target_established_local :: AbsBearerInfo
                                        -> DiffusionScript
                                        -> Property
prop_diffusion_target_established_local defaultBearerInfo diffScript =
    let sim :: forall s . IOSim s Void
        sim = diffusionSimulation (toBearerInfo defaultBearerInfo)
                                  diffScript
                                  tracersExtraWithTimeName
                                  tracerDiffusionSimWithTimeName

        events :: [Events DiffusionTestTrace]
        events = fmap ( Signal.eventsFromList
                      . fmap (\(WithName _ (WithTime t b)) -> (t, b))
                      )
               . Trace.toList
               . splitWithNameTrace
               . Trace.fromList ()
               . fmap snd
               . Signal.eventsToList
               . Signal.eventsFromListUpToTime (Time (10 * 60 * 60))
               . Trace.toList
               . fmap (\(WithTime t (WithName name b)) -> (t, WithName name (WithTime t b)))
               . withTimeNameTraceEvents
                  @DiffusionTestTrace
                  @NtNAddr
               $ runSimTrace sim

     in conjoin
      $ verify_target_established_local
      <$> events

  where
    verify_target_established_local :: Events DiffusionTestTrace -> Property
    verify_target_established_local events =
      let govLocalRootPeersSig :: Signal (Set NtNAddr)
          govLocalRootPeersSig =
            selectDiffusionPeerSelectionState
              ( LocalRootPeers.keysSet
              . Governor.localRootPeers)
              events

          govInProgressPromoteColdSig :: Signal (Set NtNAddr)
          govInProgressPromoteColdSig =
            selectDiffusionPeerSelectionState
              Governor.inProgressPromoteCold
              events

          govEstablishedPeersSig :: Signal (Set NtNAddr)
          govEstablishedPeersSig =
            selectDiffusionPeerSelectionState
              ( EstablishedPeers.toSet
              . Governor.establishedPeers)
              events

          govEstablishedFailuresSig :: Signal (Set NtNAddr)
          govEstablishedFailuresSig =
              Signal.keyedLinger
                180 -- 3 minutes  -- TODO: too eager to reconnect?
                (fromMaybe Set.empty)
            . Signal.fromEvents
            . Signal.selectEvents
                (\case TracePromoteColdFailed _ _ peer _ _ ->
                         Just (Set.singleton peer)
                       --TODO: what about TraceDemoteWarmDone ?
                       -- these are also not immediate candidates
                       -- why does the property not fail for not tracking these?
                       TraceDemoteAsynchronous status
                         | Set.null failures -> Nothing
                         | otherwise         -> Just failures
                         where
                           failures =
                             Map.keysSet (Map.filter (==PeerCold) status)
                       TracePromoteWarmFailed _ _ peer _ ->
                         Just (Set.singleton peer)
                       _ -> Nothing
                )
            . selectDiffusionPeerSelectionEvents
            $ events

          trJoinKillSig :: Signal JoinedOrKilled
          trJoinKillSig =
              Signal.fromChangeEvents Killed -- Default to TrKillingNode
            . Signal.selectEvents
                (\case TrJoiningNetwork -> Just Joined
                       TrKillingNode    -> Just Killed
                       _                -> Nothing
                )
            . selectDiffusionSimulationTrace
            $ events

          -- Signal.keyedUntil receives 2 functions one that sets start of the
          -- set signal, one that ends it and another that stops all.
          --
          -- In this particular case we want a signal that is keyed beginning
          -- on a TrJoiningNetwork and ends on TrKillingNode, giving us a Signal
          -- with the periods when a node was alive.
          trIsNodeAlive :: Signal Bool
          trIsNodeAlive =
                not . Set.null
            <$> Signal.keyedUntil (fromJoineddOrKilled (Set.singleton ())
                                                       Set.empty)
                                  (fromJoineddOrKilled Set.empty
                                                       (Set.singleton ()))
                                  (const False)
                                  trJoinKillSig

          promotionOpportunities :: Signal (Set NtNAddr)
          promotionOpportunities =
            (\local established recentFailures inProgressPromoteCold isAlive ->
              if isAlive
              then local Set.\\ established
                         Set.\\ recentFailures
                         Set.\\ inProgressPromoteCold
              else Set.empty
            ) <$> govLocalRootPeersSig
              <*> govEstablishedPeersSig
              <*> govEstablishedFailuresSig
              <*> govInProgressPromoteColdSig
              <*> trIsNodeAlive

          promotionOpportunitiesIgnoredTooLong :: Signal (Set NtNAddr)
          promotionOpportunitiesIgnoredTooLong =
            Signal.keyedTimeout
              10 -- seconds
              id
              promotionOpportunities

       in counterexample
            ("\nSignal key: (local root peers, established peers, " ++
             "recent failures, is alive, opportunities, ignored too long)\n" ++
               intercalate "\n" (map show $ eventsToList events)
            )
        $ signalProperty 20 show
              (\(_,_,_,_,_,_, tooLong) -> Set.null tooLong)
              ((,,,,,,) <$> govLocalRootPeersSig
                      <*> govEstablishedPeersSig
                      <*> govEstablishedFailuresSig
                      <*> govInProgressPromoteColdSig
                      <*> trIsNodeAlive
                      <*> promotionOpportunities
                      <*> promotionOpportunitiesIgnoredTooLong
              )

-- | A variant of
-- 'Test.Ouroboros.Network.PeerSelection.prop_governor_target_active_below'
-- but for running on Diffusion. This means it has to have in consideration the
-- the logs for all nodes running will all appear in the trace and the test
-- property should only be valid while a given node is up and running.
--
prop_diffusion_target_active_below :: AbsBearerInfo
                                   -> DiffusionScript
                                   -> Property
prop_diffusion_target_active_below defaultBearerInfo diffScript =
    let sim :: forall s . IOSim s Void
        sim = diffusionSimulation (toBearerInfo defaultBearerInfo)
                                  diffScript
                                  tracersExtraWithTimeName
                                  tracerDiffusionSimWithTimeName

        events :: [Events DiffusionTestTrace]
        events = fmap ( Signal.eventsFromList
                      . fmap (\(WithName _ (WithTime t b)) -> (t, b))
                      )
               . Trace.toList
               . splitWithNameTrace
               . Trace.fromList ()
               . fmap snd
               . Signal.eventsToList
               . Signal.eventsFromListUpToTime (Time (10 * 60 * 60))
               . Trace.toList
               . fmap (\(WithTime t (WithName name b)) -> (t, WithName name (WithTime t b)))
               . withTimeNameTraceEvents
                  @DiffusionTestTrace
                  @NtNAddr
               $ runSimTrace sim

     in conjoin
      $ verify_target_active_below
      <$> events

  where
    verify_target_active_below :: Events DiffusionTestTrace -> Property
    verify_target_active_below events =
      let govLocalRootPeersSig :: Signal (LocalRootPeers.LocalRootPeers NtNAddr)
          govLocalRootPeersSig =
            selectDiffusionPeerSelectionState Governor.localRootPeers events

          govEstablishedPeersSig :: Signal (Set NtNAddr)
          govEstablishedPeersSig =
            selectDiffusionPeerSelectionState
              (EstablishedPeers.toSet . Governor.establishedPeers)
              events

          govActivePeersSig :: Signal (Set NtNAddr)
          govActivePeersSig =
            selectDiffusionPeerSelectionState Governor.activePeers events

          govActiveFailuresSig :: Signal (Set NtNAddr)
          govActiveFailuresSig =
              Signal.keyedLinger
                180 -- 3 minutes  -- TODO: too eager to reconnect?
                (fromMaybe Set.empty)
            . Signal.fromEvents
            . Signal.selectEvents
                (\case TracePromoteWarmFailed _ _ peer _ ->
                         --TODO: the environment does not yet cause this to happen
                         -- it requires synchronous failure in the establish
                         -- action
                         Just (Set.singleton peer)
                       --TODO
                       TraceDemoteAsynchronous status
                         | Set.null failures -> Nothing
                         | otherwise         -> Just failures
                         where
                           failures = Map.keysSet (Map.filter (==PeerWarm) status)
                       _ -> Nothing
                )
            . selectDiffusionPeerSelectionEvents
            $ events

          trJoinKillSig :: Signal JoinedOrKilled
          trJoinKillSig =
              Signal.fromChangeEvents Killed -- Default to TrKillingNode
            . Signal.selectEvents
                (\case TrJoiningNetwork -> Just Joined
                       TrKillingNode    -> Just Killed
                       _                -> Nothing
                )
            . selectDiffusionSimulationTrace
            $ events

          -- Signal.keyedUntil receives 2 functions one that sets start of the
          -- set signal, one that ends it and another that stops all.
          --
          -- In this particular case we want a signal that is keyed beginning
          -- on a TrJoiningNetwork and ends on TrKillingNode, giving us a Signal
          -- with the periods when a node was alive.
          trIsNodeAlive :: Signal Bool
          trIsNodeAlive =
                not . Set.null
            <$> Signal.keyedUntil (fromJoineddOrKilled (Set.singleton ())
                                                       Set.empty)
                                  (fromJoineddOrKilled Set.empty
                                                       (Set.singleton()))
                                  (const False)
                                  trJoinKillSig

          promotionOpportunities :: Signal (Set NtNAddr)
          promotionOpportunities =
            (\local established active recentFailures isAlive ->
              if isAlive
              then Set.unions
                    [ -- There are no opportunities if we're at or above target
                      if Set.size groupActive >= target
                         then Set.empty
                         else groupEstablished Set.\\ active
                                               Set.\\ recentFailures
                    | (target, group) <- LocalRootPeers.toGroupSets local
                    , let groupActive      = group `Set.intersection` active
                          groupEstablished = group `Set.intersection` established
                    ]
              else Set.empty
            ) <$> govLocalRootPeersSig
              <*> govEstablishedPeersSig
              <*> govActivePeersSig
              <*> govActiveFailuresSig
              <*> trIsNodeAlive

          promotionOpportunitiesIgnoredTooLong :: Signal (Set NtNAddr)
          promotionOpportunitiesIgnoredTooLong =
            Signal.keyedTimeout
              10 -- seconds
              id
              promotionOpportunities

       in counterexample
            ("\nSignal key: (local, established peers, active peers, " ++
             "recent failures, opportunities, ignored too long)") $

          signalProperty 20 show
            (\(_,_,_,_,_,_,toolong) -> Set.null toolong)
            ((,,,,,,) <$> (LocalRootPeers.toGroupSets <$> govLocalRootPeersSig)
                     <*> govEstablishedPeersSig
                     <*> govActivePeersSig
                     <*> govActiveFailuresSig
                     <*> trIsNodeAlive
                     <*> promotionOpportunities
                     <*> promotionOpportunitiesIgnoredTooLong)

-- | A variant of
-- 'Test.Ouroboros.Network.PeerSelection.prop_governor_target_active_local_above'
-- but for running on Diffusion. This means it has to have in consideration the
-- the logs for all nodes running will all appear in the trace and the test
-- property should only be valid while a given node is up and running.
--
prop_diffusion_target_active_local_above :: AbsBearerInfo
                                         -> DiffusionScript
                                         -> Property
prop_diffusion_target_active_local_above defaultBearerInfo diffScript =
    let sim :: forall s . IOSim s Void
        sim = diffusionSimulation (toBearerInfo defaultBearerInfo)
                                  diffScript
                                  tracersExtraWithTimeName
                                  tracerDiffusionSimWithTimeName

        events :: [Events DiffusionTestTrace]
        events = fmap ( Signal.eventsFromList
                      . fmap (\(WithName _ (WithTime t b)) -> (t, b))
                      )
               . Trace.toList
               . splitWithNameTrace
               . Trace.fromList ()
               . fmap snd
               . Signal.eventsToList
               . Signal.eventsFromListUpToTime (Time (10 * 60 * 60))
               . Trace.toList
               . fmap (\(WithTime t (WithName name b)) -> (t, WithName name (WithTime t b)))
               . withTimeNameTraceEvents
                  @DiffusionTestTrace
                  @NtNAddr
               $ runSimTrace sim

     in conjoin
      $ verify_target_active_above
      <$> events

  where
    verify_target_active_above :: Events DiffusionTestTrace -> Property
    verify_target_active_above events =
      let govLocalRootPeersSig :: Signal (LocalRootPeers.LocalRootPeers NtNAddr)
          govLocalRootPeersSig =
            selectDiffusionPeerSelectionState Governor.localRootPeers events

          govActivePeersSig :: Signal (Set NtNAddr)
          govActivePeersSig =
            selectDiffusionPeerSelectionState Governor.activePeers events

          trJoinKillSig :: Signal JoinedOrKilled
          trJoinKillSig =
              Signal.fromChangeEvents Killed -- Default to TrKillingNode
            . Signal.selectEvents
                (\case TrJoiningNetwork -> Just Joined
                       TrKillingNode    -> Just Killed
                       _                -> Nothing
                )
            . selectDiffusionSimulationTrace
            $ events

          -- Signal.keyedUntil receives 2 functions one that sets start of the
          -- set signal, one that ends it and another that stops all.
          --
          -- In this particular case we want a signal that is keyed beginning
          -- on a TrJoiningNetwork and ends on TrKillingNode, giving us a Signal
          -- with the periods when a node was alive.
          trIsNodeAlive :: Signal Bool
          trIsNodeAlive =
                not . Set.null
            <$> Signal.keyedUntil (fromJoineddOrKilled (Set.singleton ())
                                                       Set.empty)
                                  (fromJoineddOrKilled Set.empty
                                                       (Set.singleton ()))
                                  (const False)
                                  trJoinKillSig

          demotionOpportunities :: Signal (Set NtNAddr)
          demotionOpportunities =
            (\local active isAlive ->
              if isAlive
              then Set.unions
                    [ -- There are no opportunities if we're at or below target
                      if Set.size groupActive <= target
                         then Set.empty
                         else groupActive
                    | (target, group) <- LocalRootPeers.toGroupSets local
                    , let groupActive = group `Set.intersection` active
                    ]
              else Set.empty
            ) <$> govLocalRootPeersSig
              <*> govActivePeersSig
              <*> trIsNodeAlive

          demotionOpportunitiesIgnoredTooLong :: Signal (Set NtNAddr)
          demotionOpportunitiesIgnoredTooLong =
            Signal.keyedTimeout
              10 -- seconds
              id
              demotionOpportunities

       in counterexample
            ("\nSignal key: (local peers, active peers, " ++
             "demotion opportunities, ignored too long)") $

          signalProperty 20 show
            (\(_,_,_,_,toolong) -> Set.null toolong)
            ((,,,,) <$> (LocalRootPeers.toGroupSets <$> govLocalRootPeersSig)
                   <*> govActivePeersSig
                   <*> trIsNodeAlive
                   <*> demotionOpportunities
                   <*> demotionOpportunitiesIgnoredTooLong)


-- | A variant of ouroboros-network-framework
-- 'Test.Ouroboros.Network.Server2.prop_connection_manager_valid_transitions'
-- but for running on Diffusion. This means it has to have in consideration the
-- the logs for all nodes running will all appear in the trace and the test
-- property should only be valid while a given node is up and running.
--
prop_diffusion_cm_valid_transitions :: AbsBearerInfo
                                    -> DiffusionScript
                                    -> Property
prop_diffusion_cm_valid_transitions defaultBearerInfo diffScript =
    let sim :: forall s . IOSim s Void
        sim = diffusionSimulation (toBearerInfo defaultBearerInfo)
                                  diffScript
                                  tracersExtraWithTimeName
                                  tracerDiffusionSimWithTimeName

        events :: [Trace () DiffusionTestTrace]
        events = fmap ( Trace.fromList ()
                      . fmap (\(WithName _ (WithTime _ b)) -> b))
               . Trace.toList
               . splitWithNameTrace
               . Trace.fromList ()
               . fmap snd
               . Signal.eventsToList
               . Signal.eventsFromListUpToTime (Time (10 * 60 * 60))
               . Trace.toList
               . fmap (\(WithTime t (WithName name b))
                       -> (t, WithName name (WithTime t b)))
               . withTimeNameTraceEvents
                  @DiffusionTestTrace
                  @NtNAddr
               $ runSimTrace sim

     in conjoin
      $ verify_cm_valid_transitions
      <$> events

  where
    verify_cm_valid_transitions :: Trace () DiffusionTestTrace -> Property
    verify_cm_valid_transitions events =
      let abstractTransitionEvents :: Trace () (AbstractTransitionTrace NtNAddr)
          abstractTransitionEvents =
            selectDiffusionConnectionManagerTransitionEvents events

          connectionManagerEvents :: [ConnectionManagerTrace
                                        NtNAddr
                                        (ConnectionHandlerTrace
                                          NtNVersion
                                          NtNVersionData)]
          connectionManagerEvents =
              Trace.toList
            . selectDiffusionConnectionManagerEvents
            $ events

       in mkProperty
        . bifoldMap
           ( const mempty )
           ( \ trs
            -> TestProperty {
                 tpProperty =
                     (counterexample $!
                       (  "\nconnection:\n"
                       ++ intercalate "\n" (map ppTransition trs))
                       )
                   . getAllProperty
                   . foldMap ( \ tr
                              -> AllProperty
                               . (counterexample $!
                                   (  "\nUnexpected transition: "
                                   ++ show tr)
                                   )
                               . verifyAbstractTransition
                               $ tr
                             )
                   $ trs,
                 tpNumberOfTransitions = Sum (length trs),
                 tpNumberOfConnections = Sum 1,
                 tpNumberOfPrunings    = classifyPrunings connectionManagerEvents,
                 tpNegotiatedDataFlows = [classifyNegotiatedDataFlow trs],
                 tpEffectiveDataFlows  = [classifyEffectiveDataFlow  trs],
                 tpTerminationTypes    = [classifyTermination        trs],
                 tpActivityTypes       = [classifyActivityType       trs],
                 tpTransitions         = trs
              }
           )
        . fmap (map ttTransition)
        . groupConns id
        $ abstractTransitionEvents


-- | A variant of ouroboros-network-framework
-- 'Test.Ouroboros.Network.Server2.prop_connection_manager_valid_transition_order'
-- but for running on Diffusion. This means it has to have in consideration the
-- the logs for all nodes running will all appear in the trace and the test
-- property should only be valid while a given node is up and running.
--
prop_diffusion_cm_valid_transition_order :: AbsBearerInfo
                                         -> DiffusionScript
                                         -> Property
prop_diffusion_cm_valid_transition_order defaultBearerInfo diffScript =
    let sim :: forall s . IOSim s Void
        sim = diffusionSimulation (toBearerInfo defaultBearerInfo)
                                  diffScript
                                  tracersExtraWithTimeName
                                  tracerDiffusionSimWithTimeName

        events :: [Trace () DiffusionTestTrace]
        events = fmap ( Trace.fromList ()
                      . fmap (\(WithName _ (WithTime _ b)) -> b))
               . Trace.toList
               . splitWithNameTrace
               . Trace.fromList ()
               . fmap snd
               . Signal.eventsToList
               . Signal.eventsFromListUpToTime (Time (10 * 60 * 60))
               . Trace.toList
               . fmap (\(WithTime t (WithName name b))
                       -> (t, WithName name (WithTime t b)))
               . withTimeNameTraceEvents
                  @DiffusionTestTrace
                  @NtNAddr
               $ runSimTrace sim

     in counterexample (intercalate "\n" . map show . take 30000 $ traceEvents $ runSimTrace sim)
      . conjoin
      $ verify_cm_valid_transition_order
      <$> events

  where
    verify_cm_valid_transition_order :: Trace () DiffusionTestTrace -> Property
    verify_cm_valid_transition_order events =
      let abstractTransitionEvents :: Trace () (AbstractTransitionTrace NtNAddr)
          abstractTransitionEvents =
            selectDiffusionConnectionManagerTransitionEvents events

       in getAllProperty
         . bifoldMap
            (const mempty)
            verifyAbstractTransitionOrder
         . fmap (map ttTransition)
         . groupConns id
         $ abstractTransitionEvents

-- | A variant of ouroboros-network-framework
-- 'Test.Ouroboros.Network.Server2.prop_inbound_governor_valid_transitions'
-- but for running on Diffusion. This means it has to have in consideration the
-- the logs for all nodes running will all appear in the trace and the test
-- property should only be valid while a given node is up and running.
--
prop_diffusion_ig_valid_transitions :: AbsBearerInfo
                                    -> DiffusionScript
                                    -> Property
prop_diffusion_ig_valid_transitions defaultBearerInfo diffScript =
    let sim :: forall s . IOSim s Void
        sim = diffusionSimulation (toBearerInfo defaultBearerInfo)
                                  diffScript
                                  tracersExtraWithTimeName
                                  tracerDiffusionSimWithTimeName

        events :: [Trace () DiffusionTestTrace]
        events = fmap ( Trace.fromList ()
                      . fmap (\(WithName _ (WithTime _ b)) -> b))
               . Trace.toList
               . splitWithNameTrace
               . Trace.fromList ()
               . fmap snd
               . Signal.eventsToList
               . Signal.eventsFromListUpToTime (Time (10 * 60 * 60))
               . Trace.toList
               . fmap (\(WithTime t (WithName name b))
                       -> (t, WithName name (WithTime t b)))
               . withTimeNameTraceEvents
                  @DiffusionTestTrace
                  @NtNAddr
               $ runSimTrace sim

     in conjoin
      $ verify_ig_valid_transitions
      <$> events

  where
    verify_ig_valid_transitions :: Trace () DiffusionTestTrace -> Property
    verify_ig_valid_transitions events =
      let remoteTransitionTraceEvents :: Trace () (RemoteTransitionTrace NtNAddr)
          remoteTransitionTraceEvents =
            selectDiffusionInboundGovernorTransitionEvents events

       in getAllProperty
         . bifoldMap
            ( \ _ -> AllProperty (property True) )
            ( \ TransitionTrace {ttPeerAddr = peerAddr, ttTransition = tr} ->
                  AllProperty
                . counterexample (concat [ "Unexpected transition: "
                                         , show peerAddr
                                         , " "
                                         , show tr
                                         ])
                . verifyRemoteTransition
                $ tr
            )
         $ remoteTransitionTraceEvents

-- | A variant of ouroboros-network-framework
-- 'Test.Ouroboros.Network.Server2.prop_inbound_governor_valid_transition_order'
-- but for running on Diffusion. This means it has to have in consideration the
-- the logs for all nodes running will all appear in the trace and the test
-- property should only be valid while a given node is up and running.
--
prop_diffusion_ig_valid_transition_order :: AbsBearerInfo
                                         -> DiffusionScript
                                         -> Property
prop_diffusion_ig_valid_transition_order defaultBearerInfo diffScript =
    let sim :: forall s . IOSim s Void
        sim = diffusionSimulation (toBearerInfo defaultBearerInfo)
                                  diffScript
                                  tracersExtraWithTimeName
                                  tracerDiffusionSimWithTimeName

        events :: [Trace () DiffusionTestTrace]
        events = fmap ( Trace.fromList ()
                      . fmap (\(WithName _ (WithTime _ b)) -> b))
               . Trace.toList
               . splitWithNameTrace
               . Trace.fromList ()
               . fmap snd
               . Signal.eventsToList
               . Signal.eventsFromListUpToTime (Time (10 * 60 * 60))
               . Trace.toList
               . fmap (\(WithTime t (WithName name b))
                       -> (t, WithName name (WithTime t b)))
               . withTimeNameTraceEvents
                  @DiffusionTestTrace
                  @NtNAddr
               $ runSimTrace sim

     in conjoin
      $ verify_cm_valid_transition_order
      <$> events

  where
    verify_cm_valid_transition_order :: Trace () DiffusionTestTrace -> Property
    verify_cm_valid_transition_order events =

      let remoteTransitionTraceEvents :: Trace () (RemoteTransitionTrace NtNAddr)
          remoteTransitionTraceEvents =
            selectDiffusionInboundGovernorTransitionEvents events

      in getAllProperty
        . bifoldMap
           (const mempty)
           verifyRemoteTransitionOrder
        . splitRemoteConns
        $ remoteTransitionTraceEvents

-- | A variant of ouroboros-network-framework
-- 'Test.Ouroboros.Network.Server2.prop_timeouts_enforced'
-- but for running on Diffusion. This means it has to have in consideration the
-- the logs for all nodes running will all appear in the trace and the test
-- property should only be valid while a given node is up and running.
--
-- This test tests simultaneously the ConnectionManager and InboundGovernor's
-- timeouts.
--
prop_diffusion_timeouts_enforced :: AbsBearerInfo
                                 -> DiffusionScript
                                 -> Property
prop_diffusion_timeouts_enforced defaultBearerInfo diffScript =
    let sim :: forall s . IOSim s Void
        sim = diffusionSimulation (toBearerInfo defaultBearerInfo)
                                  diffScript
                                  tracersExtraWithTimeName
                                  tracerDiffusionSimWithTimeName

        events :: [Trace () (Time, DiffusionTestTrace)]
        events = fmap ( Trace.fromList ()
                      . fmap (\(WithName _ (WithTime t b)) -> (t, b)))
               . Trace.toList
               . splitWithNameTrace
               . Trace.fromList ()
               . fmap snd
               . Signal.eventsToList
               . Signal.eventsFromListUpToTime (Time (10 * 60 * 60))
               . Trace.toList
               . fmap (\(WithTime t (WithName name b))
                       -> (t, WithName name (WithTime t b)))
               . withTimeNameTraceEvents
                  @DiffusionTestTrace
                  @NtNAddr
               $ runSimTrace sim

     in conjoin
      $ verify_timeouts
      <$> events

  where
    verify_timeouts :: Trace () (Time, DiffusionTestTrace) -> Property
    verify_timeouts events =
      let transitionSignal :: Trace () [(Time, AbstractTransitionTrace NtNAddr)]
          transitionSignal = groupConns snd
                           . selectDiffusionConnectionManagerTransitionEventsTime
                           $ events

       in getAllProperty
        $ verifyAllTimeouts transitionSignal
       where
         verifyAllTimeouts :: Trace () [(Time, AbstractTransitionTrace NtNAddr)]
                           -> AllProperty
         verifyAllTimeouts =
           bifoldMap
            (const mempty)
            (\ tr ->
              AllProperty
              $ counterexample ("\nConnection transition trace:\n"
                              ++ intercalate "\n" (map show tr)
                              )
              $ verifyTimeouts Nothing tr)

         -- verifyTimeouts checks that in all \tau transition states the timeout is
         -- respected. It does so by checking the stream of abstract transitions
         -- paired with the time they happened, for a given connection; and checking
         -- that transitions from \tau states to any other happens withing the correct
         -- timeout bounds. One note is that for the example
         -- InboundIdleState^\tau -> OutboundState^\tau -> OutboundState sequence
         -- The first transition would be fine, but for the second we need the time
         -- when we transitioned into InboundIdleState and not OutboundState.
         --
         verifyTimeouts :: Maybe (AbstractState, Time)
                        -- ^ Map of first occurence of a given \tau state
                        -> [(Time , AbstractTransitionTrace NtNAddr)]
                        -- ^ Stream of abstract transitions for a given connection
                        -- paired with the time it ocurred
                        -> Property
         -- In Diffusion tests there can be truncated transitions
         verifyTimeouts _ [] = property True
         -- If we already seen a \tau transition state
         verifyTimeouts st@(Just (state, t')) ((t, TransitionTrace _ tt@(Transition _ to)):xs) =
             let newState  = Just (to, t)
                 idleTimeout  =
                     1.1 * tProtocolIdleTimeout simTimeouts
                 outboundTimeout =
                     1.1 * tOutboundIdleTimeout simTimeouts
                 timeWaitTimeout =
                     1.1 * tTimeWaitTimeout simTimeouts
                 handshakeTimeout = case timeLimitsHandshake of
                   (ProtocolTimeLimits stLimit) ->
                     -- Should be the same but we bias to the shorter one
                     let time = min (fromMaybe 0 (stLimit (ClientAgency TokPropose)))
                                    (fromMaybe 0 (stLimit (ServerAgency TokConfirm)))
                      in time + (0.1 * time)

              in case state of
                UnnegotiatedSt _ -> case to of
                  -- Timeout terminating states
                  OutboundUniSt ->
                    counterexample (errorMsg tt t' t handshakeTimeout)
                    $ diffTime t t' <= handshakeTimeout
                    .&&. verifyTimeouts Nothing xs
                  InboundIdleSt Unidirectional ->
                    counterexample (errorMsg tt t' t handshakeTimeout)
                    $ diffTime t t' <= handshakeTimeout
                    .&&. verifyTimeouts Nothing xs
                  TerminatedSt ->
                    counterexample (errorMsg tt t' t handshakeTimeout)
                    $ diffTime t t' <= handshakeTimeout
                    .&&. verifyTimeouts Nothing xs

                  -- These states terminate the current timeout
                  -- and starts a new one
                  OutboundDupSt Ticking ->
                    counterexample (errorMsg tt t' t handshakeTimeout)
                    $ diffTime t t' <= handshakeTimeout
                    .&&. verifyTimeouts newState xs
                  InboundIdleSt Duplex ->
                    counterexample (errorMsg tt t' t handshakeTimeout)
                    $ diffTime t t' <= handshakeTimeout
                    .&&. verifyTimeouts newState xs

                  _ -> error ("Unexpected invalid transition: " ++ show (st, tt))

                InboundIdleSt Duplex         -> case to of
                  -- Should preserve the timeout
                  OutboundDupSt Ticking -> verifyTimeouts st xs
                  InboundIdleSt Duplex -> verifyTimeouts st xs

                  -- Timeout terminating states
                  OutboundDupSt Expired ->
                    counterexample (errorMsg tt t' t idleTimeout)
                    $ diffTime t t' <= idleTimeout
                    .&&. verifyTimeouts Nothing xs
                  InboundSt Duplex ->
                    counterexample (errorMsg tt t' t idleTimeout)
                    $ diffTime t t' <= idleTimeout
                    .&&. verifyTimeouts Nothing xs
                  DuplexSt ->
                    counterexample (errorMsg tt t' t idleTimeout)
                    $ diffTime t t' <= idleTimeout
                    .&&. verifyTimeouts Nothing xs
                  TerminatedSt ->
                    counterexample (errorMsg tt t' t idleTimeout)
                    $ diffTime t t' <= idleTimeout
                    .&&. verifyTimeouts Nothing xs

                  -- This state terminates the current timeout
                  -- and starts a new one
                  TerminatingSt ->
                    counterexample (errorMsg tt t' t idleTimeout)
                    $ diffTime t t' <= idleTimeout
                    .&&. verifyTimeouts newState xs

                  _ -> error ("Unexpected invalid transition: " ++ show (st, tt))

                InboundIdleSt Unidirectional -> case to of
                  -- Timeout terminating states
                  InboundSt Unidirectional ->
                    counterexample (errorMsg tt t' t idleTimeout)
                    $ diffTime t t' <= idleTimeout
                    .&&. verifyTimeouts Nothing xs
                  TerminatedSt ->
                    counterexample (errorMsg tt t' t idleTimeout)
                    $ diffTime t t' <= idleTimeout
                    .&&. verifyTimeouts Nothing xs

                  -- This state terminates the current timeout
                  -- and starts a new one
                  TerminatingSt ->
                    counterexample (errorMsg tt t' t idleTimeout)
                    $ diffTime t t' <= idleTimeout
                    .&&. verifyTimeouts newState xs

                  _ -> error ("Unexpected invalid transition: " ++ show (st, tt))

                OutboundDupSt Ticking        -> case to of
                  -- Should preserve the timeout
                  InboundIdleSt Duplex -> verifyTimeouts st xs
                  OutboundDupSt Ticking -> verifyTimeouts st xs

                  -- Timeout terminating states
                  OutboundDupSt Expired ->
                    counterexample (errorMsg tt t' t idleTimeout)
                    $ diffTime t t' <= outboundTimeout
                    .&&. verifyTimeouts Nothing xs
                  DuplexSt ->
                    counterexample (errorMsg tt t' t idleTimeout)
                    $ diffTime t t' <= outboundTimeout
                    .&&. verifyTimeouts Nothing xs
                  InboundSt Duplex ->
                    counterexample (errorMsg tt t' t idleTimeout)
                    $ diffTime t t' <= outboundTimeout
                    .&&. verifyTimeouts Nothing xs
                  TerminatedSt ->
                    counterexample (errorMsg tt t' t idleTimeout)
                    $ diffTime t t' <= outboundTimeout
                    .&&. verifyTimeouts Nothing xs

                  -- This state terminates the current timeout
                  -- and starts a new one
                  TerminatingSt ->
                    counterexample (errorMsg tt t' t idleTimeout)
                    $ diffTime t t' <= outboundTimeout
                    .&&. verifyTimeouts newState xs

                  _ -> error ("Unexpected invalid transition: " ++ show (st, tt))

                OutboundIdleSt _             -> case to of
                  -- Timeout terminating states
                  InboundSt Duplex ->
                    counterexample (errorMsg tt t' t idleTimeout)
                    $ diffTime t t' <= outboundTimeout
                    .&&. verifyTimeouts Nothing xs
                  TerminatedSt ->
                    counterexample (errorMsg tt t' t idleTimeout)
                    $ diffTime t t' <= outboundTimeout
                    .&&. verifyTimeouts Nothing xs

                  -- This state terminates the current timeout
                  -- and starts a new one
                  TerminatingSt ->
                    counterexample (errorMsg tt t' t idleTimeout)
                    $ diffTime t t' <= outboundTimeout
                    .&&. verifyTimeouts newState xs

                  _ -> error ("Unexpected invalid transition: " ++ show (st, tt))

                TerminatingSt                -> case to of
                  -- Timeout terminating states
                  UnnegotiatedSt Inbound ->
                    counterexample (errorMsg tt t' t idleTimeout)
                    $ diffTime t t' <= timeWaitTimeout
                    .&&. verifyTimeouts Nothing xs

                  TerminatedSt ->
                    counterexample (errorMsg tt t' t idleTimeout)
                    $ diffTime t t' <= timeWaitTimeout
                    .&&. verifyTimeouts Nothing xs

                  _ -> error ("Unexpected invalid transition: " ++ show (st, tt))

                _ -> error ("Should be a \tau state: " ++ show st)
           where
             errorMsg trans time' time maxDiffTime =
               "\nAt transition: " ++ show trans ++ "\n"
               ++ "First happened at: " ++ show time' ++ "\n"
               ++ "Second happened at: " ++ show time ++ "\n"
               ++ "Should only take: "
               ++ show maxDiffTime
               ++ ", but took:" ++ show (diffTime time time')
         -- If we haven't seen a \tau transition state
         verifyTimeouts Nothing ((t, TransitionTrace _ (Transition _ to)):xs) =
             let newState = Just (to, t)
              in case to of
                InboundIdleSt _       -> verifyTimeouts newState xs
                OutboundDupSt Ticking -> verifyTimeouts newState xs
                OutboundIdleSt _      -> verifyTimeouts newState xs
                TerminatingSt         -> verifyTimeouts newState xs
                _                     -> verifyTimeouts Nothing xs

-- Utils
--

data JoinedOrKilled = Joined | Killed
  deriving (Eq, Show)

-- Similar to 'either' but for 'JoinedOrKilled'
fromJoineddOrKilled :: c -> c -> JoinedOrKilled -> c
fromJoineddOrKilled j _ Joined = j
fromJoineddOrKilled _ k Killed = k

--
-- | Configurable timeouts.  We use different timeouts for 'IO' and 'IOSim' property tests.
--
data Timeouts = Timeouts {
    tProtocolIdleTimeout :: DiffTime,
    tOutboundIdleTimeout :: DiffTime,
    tTimeWaitTimeout     :: DiffTime
  }

-- | Timeouts for 'IOSim' tests.
--
simTimeouts :: Timeouts
simTimeouts = Timeouts {
    tProtocolIdleTimeout = 5,
    tOutboundIdleTimeout = 5,
    tTimeWaitTimeout     = 30
  }

dynamicTracer :: (Typeable a, Show a) => Tracer (IOSim s) a
dynamicTracer = Tracer traceM <> sayTracer

withTimeNameTraceEvents :: forall b name r. (Typeable b, Typeable name)
                        => SimTrace r
                        -> Trace (SimResult r) (WithTime (WithName name b))
withTimeNameTraceEvents = traceSelectTraceEventsDynamic
                            @r
                            @(WithTime (WithName name b))

selectDiffusionPeerSelectionEvents :: Events DiffusionTestTrace
                                   -> Events (TracePeerSelection NtNAddr)
selectDiffusionPeerSelectionEvents = Signal.selectEvents
                    (\case DiffusionPeerSelectionTrace e -> Just e
                           _                             -> Nothing)

selectDiffusionSimulationTrace :: Events DiffusionTestTrace
                               -> Events DiffusionSimulationTrace
selectDiffusionSimulationTrace = Signal.selectEvents
                    (\case DiffusionDiffusionSimulationTrace e -> Just e
                           _                                   -> Nothing)

selectDiffusionPeerSelectionState :: Eq a
                                  => (Governor.PeerSelectionState NtNAddr () -> a)
                                  -> Events DiffusionTestTrace
                                  -> Signal a
selectDiffusionPeerSelectionState f =
    Signal.nub
  . fmap f
  -- TODO: #3182 Rng seed should come from quickcheck.
  . Signal.fromChangeEvents (Governor.emptyPeerSelectionState $ mkStdGen 42)
  . Signal.selectEvents
      (\case
        DiffusionDebugPeerSelectionTrace (TraceGovernorState _ _ st) -> Just st
        _                                                            -> Nothing)

selectDiffusionConnectionManagerEvents
  :: Trace () DiffusionTestTrace
  -> Trace () (ConnectionManagerTrace NtNAddr
                 (ConnectionHandlerTrace
                    NtNVersion
                    NtNVersionData))
selectDiffusionConnectionManagerEvents =
  Trace.fromList ()
  . mapMaybe
     (\case DiffusionConnectionManagerTrace e -> Just e
            _                                 -> Nothing)
  . Trace.toList

selectDiffusionConnectionManagerTransitionEvents
  :: Trace () DiffusionTestTrace
  -> Trace () (AbstractTransitionTrace NtNAddr)
selectDiffusionConnectionManagerTransitionEvents =
  Trace.fromList ()
  . mapMaybe
     (\case DiffusionConnectionManagerTransitionTrace e -> Just e
            _                                           -> Nothing)
  . Trace.toList

selectDiffusionConnectionManagerTransitionEventsTime
  :: Trace () (Time, DiffusionTestTrace)
  -> Trace () (Time, AbstractTransitionTrace NtNAddr)
selectDiffusionConnectionManagerTransitionEventsTime =
  Trace.fromList ()
  . mapMaybe
     (\case (t, DiffusionConnectionManagerTransitionTrace e) -> Just (t, e)
            _                                                -> Nothing)
  . Trace.toList

selectDiffusionInboundGovernorTransitionEvents
  :: Trace () DiffusionTestTrace
  -> Trace () (RemoteTransitionTrace NtNAddr)
selectDiffusionInboundGovernorTransitionEvents =
  Trace.fromList ()
  . mapMaybe
     (\case DiffusionInboundGovernorTransitionTrace e -> Just e
            _                                         -> Nothing)
  . Trace.toList

toBearerInfo :: AbsBearerInfo -> BearerInfo
toBearerInfo abi =
    BearerInfo {
        biConnectionDelay      = delay (abiConnectionDelay abi),
        biInboundAttenuation   = attenuation (abiInboundAttenuation abi),
        biOutboundAttenuation  = attenuation (abiOutboundAttenuation abi),
        biInboundWriteFailure  = abiInboundWriteFailure abi,
        biOutboundWriteFailure = abiOutboundWriteFailure abi,
        biAcceptFailures       = Nothing, -- TODO
        biSDUSize              = toSduSize (abiSDUSize abi)
      }

