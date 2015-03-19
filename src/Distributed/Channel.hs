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
  ps <- forM peers $ \nid -> do
    say $ printf "spawning on %s" (show nid)
    spawn nid $(mkStaticClosure 'pingServerChannel)

  mapM_ monitor ps

  ports <- forM ps $ \pid -> do
    say $ printf "pinging %s" (show pid)
    (sendport, recvport) <- newChan
    send pid $ Ping sendport
    return recvport

  forM_ ports $ \port -> do
    _ <- receiveChan port
    return ()

  say "All pongs received"
  terminate
  
