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

import DistribMain
import qualified Channel as Channel

data Message = Ping ProcessId
             | Pong ProcessId
  deriving (Typeable, Generic)

instance Binary Message

pingServer :: Process ()
pingServer = do
  Ping from <- expect
  say $ printf "ping received from %s" (show from)
  mypid <- getSelfPid
  send from $ Pong mypid

$(remotable ['pingServer])

master :: Process ()
master = do
  node <- getSelfNode
  say $ printf "spawning on %s" (show node)
  pid <- spawn node $(mkStaticClosure 'pingServer)

  mypid <- getSelfPid
  say $ printf "sending ping to %s" (show pid)
  send pid $ Ping mypid

  Pong fromid <- expect
  say $ printf "Pong from pid: %s" (show fromid)
  terminate

masterMulti :: [NodeId] -> Process ()
masterMulti peers = do
  ps <- forM peers $ \nid -> do
    say $ printf "spawning on %s" (show nid)
    spawn nid $(mkStaticClosure 'pingServer)

  mypid <- getSelfPid

  forM ps $ \pid -> do
    say $ printf "sending ping to %s" (show pid)
    send pid $ Ping mypid

  waitForPongs ps

  say "All pongs received"
  terminate
  where
    waitForPongs :: [ProcessId] -> Process ()
    waitForPongs [] = return ()
    waitForPongs ps = do
      m <- expect
      case m of
        Pong p -> waitForPongs $ filter (/= p) ps
        _ -> say "master received ping" >> terminate
  
main :: IO ()
main = do
  (command:args) <- getArgs
  case command of
    "simple" -> withArgs args $ distribMain (\_ -> master) Main.__remoteTable
    "multi" -> withArgs args $ distribMain  masterMulti Main.__remoteTable
    "channel" -> withArgs args $ distribMain Channel.master Main.__remoteTable

