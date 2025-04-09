{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Indexer.ChainSync
  ( runNodeClient,
  ) where

import Cardano.Indexer.Config (App, StandardBlock)
import Cardano.Indexer.Config qualified as Cfg

import Cardano.BM.Data.LogItem (LoggerName)
import Cardano.BM.Trace (Trace, appendName)
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
import Control.Tracer (nullTracer)
import Network.Mux (Mode (..))
import Network.TypedProtocol (IsPipelined (..), PeerRole (..), Protocol (..))
import Network.TypedProtocol.Codec (Codec)
import Network.TypedProtocol.Peer (Peer)
import Network.TypedProtocol.Stateful.Codec qualified as Stateful
import Network.TypedProtocol.Stateful.Peer qualified as Stateful
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.Config (configCodec)
import Ouroboros.Consensus.Network.NodeToClient (ClientCodecs, Codecs' (..), clientCodecs)
import Ouroboros.Consensus.Node (ProtocolInfo (..))
import Ouroboros.Consensus.Node.NetworkProtocolVersion (BlockNodeToClientVersion)
import Ouroboros.Consensus.Node.NetworkProtocolVersion qualified as ProtoVersion
import Ouroboros.Consensus.Protocol.Praos ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Util (ShowProxy)
import Ouroboros.Network.Block (Point, Tip)
import Ouroboros.Network.Driver.Stateful qualified as Stateful
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.Mux (RunMiniProtocolWithMinimalCtx)
import Ouroboros.Network.Mux qualified as Mux
import Ouroboros.Network.NodeToClient (LocalAddress, NodeToClientVersion, TraceSendRecv)
import Ouroboros.Network.NodeToClient qualified as NodeToClient
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
instance Transformable Text IO (SubscriptionTrace ())

instance Transformable Text IO (TraceSendRecv (ChainSync blk (Point blk) (Tip blk)))

runNodeClient :: App ()
runNodeClient = do
  magicId <- asks (Cfg.networkMagicId . Cfg.cfgMagic)
  socketPath <- asks Cfg.cfgSocketPath
  protoInfo <- asks Cfg.cfgProtocolInfo
  tracer <- asks Cfg.cfgTrace

  liftIO $
    NodeToClient.withIOManager $ \ioManager ->
      Subscription.subscribe
        (NodeToClient.localSnocket ioManager)
        (NetworkMagic magicId)
        nodeToClientVersions
        (subscriptionTracers tracer)
        (params socketPath)
        (protocols tracer protoInfo)

nodeToClientVersions :: Map NodeToClientVersion (BlockNodeToClientVersion StandardBlock)
nodeToClientVersions = ProtoVersion.supportedNodeToClientVersions (Proxy @StandardBlock)

subscriptionTracers :: Trace IO Text -> SubscriptionTracers ()
subscriptionTracers tracer =
  Subscription.SubscriptionTracers
    { stMuxTracer = mkTracer tracer "Mux",
      stHandshakeTracer = mkTracer tracer "Handshake",
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
    whenComplete (Left _) = Subscription.Reconnect
    whenComplete _ = Subscription.Abort

protocols
  :: Trace IO Text
  -> ProtocolInfo Cfg.StandardBlock
  -> NodeToClientVersion
  -> BlockNodeToClientVersion Cfg.StandardBlock
  -> InitiatorProtocols () Void
protocols tracer protoInfo clientVersion blockVersion =
  NodeToClient.NodeToClientProtocols
    { localChainSyncProtocol = localChainSyncProtocol tracer codecs,
      localTxSubmissionProtocol = localTxSubmissionProtocol codecs,
      localStateQueryProtocol = localStateQueryProtocol codecs,
      localTxMonitorProtocol = localTxMonitorProtocol codecs
    }
  where
    codecs = codecConfig protoInfo blockVersion clientVersion

localChainSyncProtocol
  :: Trace IO Text
  -> ClientCodecs Cfg.StandardBlock IO
  -> InitiatorRunMiniProtocol () Void
localChainSyncProtocol tracer codecs = mkInitiatorProtocolOnly tracer' codec peer
  where
    tracer' = Logging.toLogObject (appendName "ChainSync" tracer)
    codec = cChainSyncCodec codecs
    peer = NodeToClient.chainSyncPeerNull

localTxSubmissionProtocol
  :: ClientCodecs Cfg.StandardBlock IO
  -> InitiatorRunMiniProtocol () Void
localTxSubmissionProtocol codecs = mkInitiatorProtocolOnly nullTracer codec peer
  where
    codec = cTxSubmissionCodec codecs
    peer = NodeToClient.localTxSubmissionPeerNull

localStateQueryProtocol
  :: ClientCodecs Cfg.StandardBlock IO
  -> InitiatorRunMiniProtocol () Void
localStateQueryProtocol codecs = mkInitiatorProtocolOnlySt st nullTracer codec peer
  where
    codec = cStateQueryCodec codecs
    peer = NodeToClient.localStateQueryPeerNull
    st = StateIdle

localTxMonitorProtocol
  :: ClientCodecs Cfg.StandardBlock IO
  -> InitiatorRunMiniProtocol () Void
localTxMonitorProtocol codecs = mkInitiatorProtocolOnly nullTracer codec peer
  where
    codec = cTxMonitorCodec codecs
    peer = NodeToClient.localTxMonitorPeerNull

codecConfig
  :: ProtocolInfo Cfg.StandardBlock
  -> BlockNodeToClientVersion Cfg.StandardBlock
  -> NodeToClientVersion
  -> ClientCodecs Cfg.StandardBlock IO
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
