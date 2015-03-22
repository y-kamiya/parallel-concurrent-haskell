{-# Language TemplateHaskell #-}
{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}

import System.Environment
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
import Control.Concurrent (threadDelay)

import DistribMain

data Message = Ping (SendPort String) deriving (Typeable, Generic)

instance Binary Message

pingServer :: Process ()
pingServer = do
  Ping chan <- expect
  -- liftIO $ threadDelay 5000000
  -- say $ printf "ping received from %s" (show chan)
  mypid <- getSelfPid
  sendChan chan $ "pong from " ++ show mypid

$(remotable ['pingServer])

master :: [NodeId] -> Process ()
master peers = do
  let flags = TT.defaultTraceFlags { TT.traceDied = Just TT.TraceAll 
                                   -- , TT.traceSend = Just TT.TraceAll
                                   -- , TT.traceRecv = Just TT.TraceAll
                                   -- , TT.traceNodes = True
                                   -- , TT.traceConnections = True
                                   }
  ps <- forM peers $ \nid -> do
    say $ printf "spawning on %s" (show nid)
    TR.setTraceFlagsRemote flags nid
    _ <- TR.startTraceRelay nid 
    spawn nid $(mkStaticClosure 'pingServer)

  ports <- forM ps $ \pid -> do
    say $ printf "pinging %s" (show pid)
    (sp, rp) <- newChan
    send pid $ Ping sp
    -- withMonitor pid $ do
    --   send pid $ Ping sp
    --   receiveWait
    --     [ match $ \(ProcessMonitorNotification _ deadpid reason) -> do
    --         say $ printf "process %s died: %s" (show deadpid) (show reason)
    --         terminate
    --     ]
    return rp

  forM_ ports $ \port -> do
    m <- receiveChan port
    say $ show m
    return ()

  say "All pongs received"
  terminate
  
main :: IO ()
main = do
  (command:args) <- getArgs
  case command of
    "channel" -> withArgs args $ distribMain master Main.__remoteTable

