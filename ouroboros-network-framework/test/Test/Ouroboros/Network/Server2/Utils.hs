{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Ouroboros.Network.Server2.Utils where

import           Control.Monad.Class.MonadThrow
import           Control.Monad.IOSim
import           Control.Tracer (Tracer (..))

import           Data.List (dropWhileEnd, find)
import qualified Data.List.Trace as Trace
import           Data.Monoid (Sum (..))
import           Data.Typeable (Typeable)

import           Text.Printf

import           Test.QuickCheck

import           Ouroboros.Network.ConnectionHandler
import           Ouroboros.Network.ConnectionManager.Types
import           Ouroboros.Network.Mux
import           Ouroboros.Network.Snocket (TestAddress (..))
import qualified Ouroboros.Network.Snocket as Snocket

import           Simulation.Network.Snocket

import           Ouroboros.Network.Testing.Data.AbsBearerInfo
                     (AbsBearerInfo (..),
                     AbsBearerInfoScript (..),
                     NonFailingAbsBearerInfoScript (..),
                     toNonFailingAbsBearerInfoScript)
import           Ouroboros.Network.Testing.Utils
                     (WithName (..), WithTime (..), sayTracer)

import           Test.Ouroboros.Network.Orphans ()

-- | Test property together with classification.
--
data TestProperty = TestProperty {
    tpProperty            :: !Property,
    -- ^ 'True' if property is true

    tpNumberOfTransitions :: !(Sum Int),
    -- ^ number of all transitions

    tpNumberOfConnections :: !(Sum Int),
    -- ^ number of all connections

    tpNumberOfPrunings    :: !(Sum Int),
    -- ^ number of all connections

    --
    -- classification of connections
    --
    tpNegotiatedDataFlows :: ![NegotiatedDataFlow],
    tpEffectiveDataFlows  :: ![EffectiveDataFlow],
    tpTerminationTypes    :: ![TerminationType],
    tpActivityTypes       :: ![ActivityType],

    tpTransitions         :: ![AbstractTransition]

  }

instance Show TestProperty where
    show tp =
      concat [ "TestProperty "
             , "{ tpNumberOfTransitions = " ++ show (tpNumberOfTransitions tp)
             , ", tpNumberOfConnections = " ++ show (tpNumberOfConnections tp)
             , ", tpNumberOfPrunings = "    ++ show (tpNumberOfPrunings tp)
             , ", tpNegotiatedDataFlows = " ++ show (tpNegotiatedDataFlows tp)
             , ", tpTerminationTypes = "    ++ show (tpTerminationTypes tp)
             , ", tpActivityTypes = "       ++ show (tpActivityTypes tp)
             , ", tpTransitions = "         ++ show (tpTransitions tp)
             , "}"
             ]

instance Semigroup TestProperty where
  (<>) (TestProperty a0 a1 a2 a3 a4 a5 a6 a7 a8)
       (TestProperty b0 b1 b2 b3 b4 b5 b6 b7 b8) =
      TestProperty (a0 .&&. b0)
                   (a1 <> b1)
                   (a2 <> b2)
                   (a3 <> b3)
                   (a4 <> b4)
                   (a5 <> b5)
                   (a6 <> b6)
                   (a7 <> b7)
                   (a8 <> b8)

instance Monoid TestProperty where
    mempty = TestProperty (property True)
                          mempty mempty mempty mempty
                          mempty mempty mempty mempty

mkProperty :: TestProperty -> Property
mkProperty TestProperty { tpProperty
                        , tpNumberOfTransitions = Sum numberOfTransitions_
                        , tpNumberOfConnections = Sum numberOfConnections_
                        , tpNumberOfPrunings = Sum numberOfPrunings_
                        , tpNegotiatedDataFlows
                        , tpEffectiveDataFlows
                        , tpTerminationTypes
                        , tpActivityTypes
                        , tpTransitions
                        } =
     label (concat [ "Number of transitions: "
                   , within_ 10 numberOfTransitions_
                   ]
           )
   . label (concat [ "Number of connections: "
                   , show numberOfConnections_
                   ]
           )
   . tabulate "Pruning"             [show numberOfPrunings_]
   . tabulate "Negotiated DataFlow" (map show tpNegotiatedDataFlows)
   . tabulate "Effective DataFLow"  (map show tpEffectiveDataFlows)
   . tabulate "Termination"         (map show tpTerminationTypes)
   . tabulate "Activity Type"       (map show tpActivityTypes)
   . tabulate "Transitions"         (map ppTransition tpTransitions)
   $ tpProperty

mkPropertyPruning :: TestProperty -> Property
mkPropertyPruning tp@TestProperty { tpNumberOfPrunings = Sum numberOfPrunings_ } =
     cover 35 (numberOfPrunings_ > 0) "Prunings"
   . mkProperty
   $ tp

newtype AllProperty = AllProperty { getAllProperty :: Property }

instance Semigroup AllProperty where
    AllProperty a <> AllProperty b = AllProperty (a .&&. b)

instance Monoid AllProperty where
    mempty = AllProperty (property True)

newtype ArbDataFlow = ArbDataFlow DataFlow
  deriving Show

instance Arbitrary ArbDataFlow where
    arbitrary = ArbDataFlow <$> frequency [ (3, pure Duplex)
                                          , (1, pure Unidirectional)
                                          ]
    shrink (ArbDataFlow Duplex)         = [ArbDataFlow Unidirectional]
    shrink (ArbDataFlow Unidirectional) = []

data ActivityType
    = IdleConn

    -- | Active connections are onces that reach any of the state:
    --
    -- - 'InboundSt'
    -- - 'OutobundUniSt'
    -- - 'OutboundDupSt'
    -- - 'DuplexSt'
    --
    | ActiveConn
    deriving (Eq, Show)

data TerminationType
    = ErroredTermination
    | CleanTermination
    deriving (Eq, Show)

data NegotiatedDataFlow
    = NotNegotiated

    -- | Negotiated value of 'DataFlow'
    | NegotiatedDataFlow DataFlow
    deriving (Eq, Show)

data EffectiveDataFlow
    -- | Unlike the negotiated 'DataFlow' this indicates if the connection has
    -- ever been in 'DuplexSt'
    --
    = EffectiveDataFlow DataFlow
    deriving (Eq, Show)


data Name addr = Client addr
               | Node addr
               | MainServer
  deriving Eq

instance Show addr => Show (Name addr) where
    show (Client addr) = "client-" ++ show addr
    show (Node   addr) = "node-"   ++ show addr
    show  MainServer   = "main-server"

-- | The concrete address type used by simulations.
--
type SimAddr  = Snocket.TestAddress SimAddr_
type SimAddr_ = Int

-- | We use a wrapper for test addresses since the Arbitrary instance for Snocket.TestAddress only
--   generates addresses between 1 and 4.
newtype TestAddr = TestAddr { unTestAddr :: SimAddr }
  deriving (Show, Eq, Ord)

instance Arbitrary TestAddr where
  arbitrary = TestAddr . Snocket.TestAddress <$> choose (1, 100)

-- | Each node in the multi-node experiment is controlled by a thread responding to these messages.
data ConnectionHandlerMessage peerAddr req
  = NewConnection peerAddr
    -- ^ Connect to the server at the given address.
  | Disconnect peerAddr
    -- ^ Disconnect from the server at the given address.
  | RunMiniProtocols peerAddr (Bundle [req])
    -- ^ Run a bundle of mini protocols against the server at the given address (requires an active
    --   connection).
  | Shutdown
    -- ^ Shutdowns a server at the given address



data ExperimentError addr =
      NodeNotRunningException addr
    | NoActiveConnection addr addr
    | SimulationTimeout
  deriving (Typeable, Show)

instance ( Show addr, Typeable addr ) => Exception (ExperimentError addr)


dynamicTracer :: (Typeable a, Show a) => Tracer (IOSim s) a
dynamicTracer = Tracer traceM <> sayTracer

toNonFailing :: Script AbsBearerInfo -> Script AbsBearerInfo
toNonFailing = unNFBIScript
             . toNonFailingAbsBearerInfoScript
             . AbsBearerInfoScript

traceWithNameTraceEvents :: forall b. Typeable b
                    => SimTrace () -> Trace (SimResult ()) b
traceWithNameTraceEvents = fmap wnEvent
          . Trace.filter ((MainServer ==) . wnName)
          . traceSelectTraceEventsDynamic
              @()
              @(WithName (Name SimAddr) b)

withNameTraceEvents :: forall b. Typeable b => SimTrace () -> [b]
withNameTraceEvents = fmap wnEvent
          . filter ((MainServer ==) . wnName)
          . selectTraceEventsDynamic
              @()
              @(WithName (Name SimAddr) b)

withTimeNameTraceEvents :: forall b. Typeable b => SimTrace ()
                        -> Trace (SimResult ()) (WithTime b)
withTimeNameTraceEvents = fmap (\(WithTime t (WithName _ e)) -> WithTime t e)
          . Trace.filter ((MainServer ==) . wnName . wtEvent)
          . traceSelectTraceEventsDynamic
              @()
              @(WithTime (WithName (Name SimAddr) b))

-- classify negotiated data flow
classifyPrunings :: [ConnectionManagerTrace
                      addr
                      (ConnectionHandlerTrace
                        version versionData)]
                 -> Sum Int
classifyPrunings =
  Sum
  . length
  . filter ( \ tr
             -> case tr of
                  x -> case x of
                    TrPruneConnections _ _ _ -> True
                    _                        -> False
           )

-- classify negotiated data flow
classifyNegotiatedDataFlow :: [AbstractTransition] -> NegotiatedDataFlow
classifyNegotiatedDataFlow as =
  case find ( \ tr
             -> case toState tr of
                  OutboundUniSt    -> True
                  OutboundDupSt {} -> True
                  InboundIdleSt {} -> True
                  _                -> False
            ) as of
     Nothing -> NotNegotiated
     Just tr ->
       case toState tr of
         OutboundUniSt      -> NegotiatedDataFlow Unidirectional
         OutboundDupSt {}   -> NegotiatedDataFlow Duplex
         (InboundIdleSt df) -> NegotiatedDataFlow df
         _                  -> error "impossible happened!"

-- classify effective data flow
classifyEffectiveDataFlow :: [AbstractTransition] -> EffectiveDataFlow
classifyEffectiveDataFlow as =
  case find ((== DuplexSt) . toState) as of
    Nothing -> EffectiveDataFlow Unidirectional
    Just _  -> EffectiveDataFlow Duplex

-- classify termination
classifyTermination :: [AbstractTransition] -> TerminationType
classifyTermination as =
  case last $ dropWhileEnd
                (== (Transition TerminatedSt TerminatedSt))
            $ dropWhileEnd
                (== (Transition TerminatedSt UnknownConnectionSt))
            $ as of
    Transition { fromState = TerminatingSt
               , toState   = TerminatedSt
               } -> CleanTermination
    _            -> ErroredTermination

-- classify if a connection is active or not
classifyActivityType :: [AbstractTransition] -> ActivityType
classifyActivityType as =
  case find ( \ tr
             -> case toState tr of
                  InboundSt     {} -> True
                  OutboundUniSt    -> True
                  OutboundDupSt {} -> True
                  DuplexSt      {} -> True
                  _                -> False
            ) as of
    Nothing -> IdleConn
    Just {} -> ActiveConn

ppTransition :: AbstractTransition -> String
ppTransition Transition {fromState, toState} =
    printf "%-30s → %s" (show fromState) (show toState)

within_ :: Int -> Int -> String
within_ _ 0 = "0"
within_ a b = let x = b `div` a in
              concat [ if b < a
                         then "1"
                         else show $ x * a
                     , " - "
                     , show $ x * a + a - 1
                     ]
