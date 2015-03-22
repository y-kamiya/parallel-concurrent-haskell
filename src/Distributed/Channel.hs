{-# Language TemplateHaskell #-}
{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}

module Channel where

import Text.Printf
import Data.Typeable
import Data.Binary
import GHC.Generics
import Control.Monad
import Control.Distributed.Process hiding (Message(..))
import Control.Distributed.Process.Serializable
import Control.Distributed.Process.Internal.Closure.TH
import qualified Control.Distributed.Process.Management.Internal.Trace.Remote as TR
import qualified Control.Distributed.Process.Management.Internal.Trace.Types as TT


data Message = Ping (SendPort ProcessId) deriving (Typeable, Generic)

instance Binary Message

pingServerChannel :: Process ()
pingServerChannel = do
  Ping chan <- expect
  say $ printf "ping received from %s" (show chan)
  mypid <- getSelfPid
  sendChan chan mypid

$(remotable ['pingServerChannel])

master :: [NodeId] -> Process ()
master peers = do
  let flags = TT.defaultTraceFlags { TT.traceDied = Just TT.TraceAll }
  ps <- forM peers $ \nid -> do
    say $ printf "spawning on %s" (show nid)
    TR.setTraceFlagsRemote flags nid
    _ <- TR.startTraceRelay nid 
    spawn nid $(mkStaticClosure 'pingServerChannel)

  ports <- forM ps $ \pid -> do
    say $ printf "pinging %s" (show pid)
    (sp, rp) <- newChan
    withMonitor pid $ do
      send pid $ Ping sp
      receiveWait
        [ match $ \(ProcessMonitorNotification _ deadpid reason) -> do
            say $ printf "process %s died: %s" (show deadpid) (show reason)
            terminate
        ]
    return rp

  forM_ ports $ \port -> do
    _ <- receiveChan port
    return ()

  say "All pongs received"
  terminate
  
