{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Indexer.ChainSync
  ( runNodeClient,
  ) where

import Cardano.Indexer.Config
  ( App,
    ReactorQueue,
    StandardBlock,
    StandardPoint,
    StandardTip,
  )
import Cardano.Indexer.Config qualified as Cfg

import Cardano.BM.Data.LogItem (LoggerName)
import Cardano.BM.Data.LogItem qualified as Logging
import Cardano.BM.Trace (Trace, appendName, logError)
import Cardano.BM.Tracing
  ( HasPrivacyAnnotation,
    HasSeverityAnnotation,
    ToObject,
    Tracer,
    Transformable,
  )
import Cardano.BM.Tracing qualified as Logging
import Cardano.Client.Subscription
  ( Decision,
    NodeToClientProtocols,
    SubscriptionParams,
    SubscriptionTrace,
    SubscriptionTracers,
  )
import Cardano.Client.Subscription qualified as Subscription
import Cardano.Tracing.OrphanInstances.Network ()
import Control.Concurrent.Class.MonadSTM.Strict qualified as STM
import Control.Monad.Extra (whenJust)
import Control.Tracer (nullTracer)
import Data.List.NonEmpty qualified as NonEmpty
import Network.Mux (Mode (..))
import Network.TypedProtocol (IsPipelined (..), PeerRole (..), Protocol (..))
import Network.TypedProtocol.Codec (Codec)
import Network.TypedProtocol.Peer (Peer)
import Network.TypedProtocol.Stateful.Codec qualified as Stateful
import Network.TypedProtocol.Stateful.Peer qualified as Stateful
import Ouroboros.Consensus.Block (Point (..))
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.Config (configCodec)
import Ouroboros.Consensus.Network.NodeToClient (ClientCodecs, Codecs' (..), clientCodecs)
import Ouroboros.Consensus.Node (ProtocolInfo (..))
import Ouroboros.Consensus.Node.NetworkProtocolVersion (BlockNodeToClientVersion)
import Ouroboros.Consensus.Node.NetworkProtocolVersion qualified as ProtoVersion
import Ouroboros.Consensus.Protocol.Praos ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Util (ShowProxy)
import Ouroboros.Network.Block (Tip, genesisPoint)
import Ouroboros.Network.Driver.Stateful qualified as Stateful
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.Mux (RunMiniProtocolWithMinimalCtx)
import Ouroboros.Network.Mux qualified as Mux
import Ouroboros.Network.NodeToClient (LocalAddress, NodeToClientVersion, TraceSendRecv)
import Ouroboros.Network.NodeToClient qualified as NodeToClient
import Ouroboros.Network.Protocol.ChainSync.Client
  ( ClientStIdle,
    ClientStIntersect,
    ClientStNext,
  )
import Ouroboros.Network.Protocol.ChainSync.Client qualified as ChainSync
import Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import Ouroboros.Network.Protocol.LocalStateQuery.Type (State (..))

type InitiatorProtocols =
  NodeToClientProtocols
    'InitiatorMode
    Subscription.LocalAddress
    LByteString
    IO

type InitiatorRunMiniProtocol a b =
  RunMiniProtocolWithMinimalCtx
    'InitiatorMode
    LocalAddress
    LByteString
    IO
    a
    b

instance HasPrivacyAnnotation (SubscriptionTrace ())
instance HasSeverityAnnotation (SubscriptionTrace ())

instance Transformable Text IO (SubscriptionTrace ()) where
  trTransformer _ (Logging.Tracer tracer) = Logging.Tracer $ \tracers -> do
    meta <-
      Logging.mkLOMeta
        (Logging.getSeverityAnnotation tracers)
        (Logging.getPrivacyAnnotation tracers)

    let
      msg = Logging.LogMessage (logMessage tracers)
      loggerName = mempty
      obj = Logging.LogObject loggerName meta msg

    Logging.traceWith (Logging.Tracer tracer) (loggerName, obj)
    where
      logMessage (Subscription.SubscriptionError err) = show err
      logMessage (Subscription.SubscriptionResult a) = "result " <> show a
      logMessage Subscription.SubscriptionReconnect = "reconnecting"
      logMessage Subscription.SubscriptionTerminate = "terminating"

instance Transformable Text IO (TraceSendRecv (ChainSync blk (Point blk) (Tip blk)))

runNodeClient :: App ()
runNodeClient = do
  magicId <- asks (Cfg.networkMagicId . Cfg.cfgMagic)
  socketPath <- asks Cfg.cfgSocketPath
  protoInfo <- asks Cfg.cfgProtocolInfo
  tracer <- asks Cfg.cfgTrace
  queue <- asks Cfg.cfgEvents

  liftIO $ do
    logError (appendName "ChainSync" tracer) "Starting chainsync client"

    NodeToClient.withIOManager $ \ioManager ->
      Subscription.subscribe
        (NodeToClient.localSnocket ioManager)
        (NetworkMagic magicId)
        nodeToClientVersions
        (subscriptionTracers tracer)
        (params socketPath)
        (protocols tracer protoInfo queue)

nodeToClientVersions :: Map NodeToClientVersion (BlockNodeToClientVersion StandardBlock)
nodeToClientVersions = ProtoVersion.supportedNodeToClientVersions (Proxy @StandardBlock)

subscriptionTracers :: Trace IO Text -> SubscriptionTracers ()
subscriptionTracers tracer =
  Subscription.SubscriptionTracers
    { stMuxTracer = nullTracer,
      stHandshakeTracer = nullTracer,
      stSubscriptionTracer = mkTracer tracer "Subscription"
    }
  where
    mkTracer
      :: (ToObject a, Transformable a IO b)
      => Trace IO a
      -> LoggerName
      -> Tracer IO b
    mkTracer tracer' name = Logging.toLogObject (appendName name tracer')

params :: Cfg.SocketPath -> SubscriptionParams ()
params (Cfg.SocketPath path) =
  Subscription.SubscriptionParams
    { spAddress = NodeToClient.LocalAddress path,
      spReconnectionDelay = Nothing, -- Use default (5 seconds)
      spCompleteCb = whenComplete
    }
  where
    whenComplete :: Either SomeException a -> Decision
    whenComplete (Left _) = Subscription.Abort
    whenComplete (Right _) = Subscription.Abort

protocols
  :: Trace IO Text
  -> ProtocolInfo StandardBlock
  -> ReactorQueue IO
  -> NodeToClientVersion
  -> BlockNodeToClientVersion StandardBlock
  -> InitiatorProtocols () Void
protocols tracer protoInfo queue clientVersion blockVersion =
  NodeToClient.NodeToClientProtocols
    { localChainSyncProtocol = localChainSyncProtocol tracer queue codecs,
      localTxSubmissionProtocol = localTxSubmissionProtocol codecs,
      localStateQueryProtocol = localStateQueryProtocol codecs,
      localTxMonitorProtocol = localTxMonitorProtocol codecs
    }
  where
    codecs = codecConfig protoInfo blockVersion clientVersion

localChainSyncProtocol
  :: Trace IO Text
  -> ReactorQueue IO
  -> ClientCodecs StandardBlock IO
  -> InitiatorRunMiniProtocol () Void
localChainSyncProtocol tracer queue codecs = mkInitiatorProtocolOnly tracer' codec peer
  where
    tracer' = Logging.toLogObject trace'
    trace' = appendName "ChainSync" tracer
    codec = cChainSyncCodec codecs
    peer =
      ChainSync.chainSyncClientPeer $
        ChainSync.ChainSyncClient $
          mkChainSyncClient trace' queue (NonEmpty.singleton genesisPoint)

localTxSubmissionProtocol
  :: ClientCodecs StandardBlock IO
  -> InitiatorRunMiniProtocol () Void
localTxSubmissionProtocol codecs = mkInitiatorProtocolOnly nullTracer codec peer
  where
    codec = cTxSubmissionCodec codecs
    peer = NodeToClient.localTxSubmissionPeerNull

localStateQueryProtocol
  :: ClientCodecs StandardBlock IO
  -> InitiatorRunMiniProtocol () Void
localStateQueryProtocol codecs = mkInitiatorProtocolOnlySt st nullTracer codec peer
  where
    codec = cStateQueryCodec codecs
    peer = NodeToClient.localStateQueryPeerNull
    st = StateIdle

localTxMonitorProtocol
  :: ClientCodecs StandardBlock IO
  -> InitiatorRunMiniProtocol () Void
localTxMonitorProtocol codecs = mkInitiatorProtocolOnly nullTracer codec peer
  where
    codec = cTxMonitorCodec codecs
    peer = NodeToClient.localTxMonitorPeerNull

mkChainSyncClient
  :: Trace IO Text
  -> ReactorQueue IO
  -> NonEmpty StandardPoint
  -> IO (ClientStIdle StandardBlock StandardPoint StandardTip IO a)
mkChainSyncClient tracer queue points =
  pure $ ChainSync.SendMsgFindIntersect points' findIntersectClient
  where
    points' = toList points
    findIntersectClient = mkFindIntersectClient tracer queue

mkFindIntersectClient
  :: Trace IO Text
  -> ReactorQueue IO
  -> ClientStIntersect StandardBlock StandardPoint StandardTip IO a
mkFindIntersectClient tracer queue =
  ChainSync.ClientStIntersect
    { recvMsgIntersectFound = const requestNextClient,
      recvMsgIntersectNotFound = requestNextClient
    }
  where
    requestNextClient =
      ChainSync.ChainSyncClient . mkRequestNextClient tracer queue Nothing

mkRequestNextClient
  :: Trace IO Text
  -> ReactorQueue IO
  -> Maybe StandardBlock
  -> Tip StandardBlock
  -> IO (ClientStIdle StandardBlock StandardPoint StandardTip IO a)
mkRequestNextClient tracer queue clientBlock serverTip = do
  whenJust clientBlock $ \block -> do
    let
      event = Cfg.WriteBlock (Cfg.ServerTip serverTip) block

    atomically $ STM.writeTBQueue (Cfg.unReactorQueue queue) event

  pure $ ChainSync.SendMsgRequestNext mempty (mkClientStNext tracer queue)

mkClientStNext
  :: Trace IO Text
  -> ReactorQueue IO
  -> ClientStNext StandardBlock StandardPoint StandardTip IO a
mkClientStNext tracer queue =
  ChainSync.ClientStNext
    { recvMsgRollForward = rollForward,
      recvMsgRollBackward = rollBackward
    }
  where
    rollForward header tip =
      ChainSync.ChainSyncClient $ mkRequestNextClient tracer queue (Just header) tip

    rollBackward point tip =
      ChainSync.ChainSyncClient $ mkRollBackClient tracer queue point tip

mkRollBackClient
  :: Trace IO Text
  -> ReactorQueue IO
  -> StandardPoint
  -> Tip StandardBlock
  -> IO (ClientStIdle StandardBlock StandardPoint StandardTip IO a)
mkRollBackClient tracer queue point serverTip = do
  res <- STM.newEmptyTMVarIO

  let
    -- Create the rollback event
    event = Cfg.RollbackBlock (Cfg.ServerTip serverTip) point whenComplete
    -- Notify us when the rollback is completed, by writing the actual block we rolled back to
    -- into an MVar. When it's filled we know the work is complete.
    whenComplete _ newPoint = atomically $ STM.putTMVar res newPoint

  -- Write the rollback event
  atomically $ STM.writeTBQueue (Cfg.unReactorQueue queue) event
  -- Wait for it to complete
  newPoint <- atomically $ STM.takeTMVar res

  -- If we rolled back to the same point as the chain sync client, we can start processing blocks
  -- right away. Otherwise, we'll need to notify the server which point we need to continue from.
  if newPoint == point
    then pure $ ChainSync.SendMsgRequestNext mempty (mkClientStNext tracer queue)
    else
      pure $
        ChainSync.SendMsgFindIntersect [newPoint] (mkFindIntersectClient tracer queue)

codecConfig
  :: ProtocolInfo StandardBlock
  -> BlockNodeToClientVersion StandardBlock
  -> NodeToClientVersion
  -> ClientCodecs StandardBlock IO
codecConfig = clientCodecs . configCodec . pInfoConfig

mkInitiatorProtocolOnly
  :: ( ShowProxy ps,
       Show failure,
       forall (st :: ps) stok. (stok ~ StateToken st) => Show stok
     )
  => Tracer IO (TraceSendRecv ps)
  -> Codec ps failure IO LByteString
  -> Peer ps 'AsClient 'NonPipelined pr IO a
  -> InitiatorRunMiniProtocol a Void
mkInitiatorProtocolOnly tracer codec peer =
  Subscription.InitiatorProtocolOnly $
    Mux.mkMiniProtocolCbFromPeer $
      const (tracer, codec, peer)

mkInitiatorProtocolOnlySt
  :: ( ShowProxy ps,
       Show failure,
       forall (st' :: ps) stok. (stok ~ StateToken st') => Show stok
     )
  => f st
  -> Tracer IO (Stateful.TraceSendRecv ps f)
  -> Stateful.Codec ps failure f IO LByteString
  -> Stateful.Peer ps pr st f IO a
  -> InitiatorRunMiniProtocol a Void
mkInitiatorProtocolOnlySt st tracer codec peer =
  Subscription.InitiatorProtocolOnly $
    Mux.mkMiniProtocolCbFromPeerSt $
      const (tracer, codec, st, peer)
