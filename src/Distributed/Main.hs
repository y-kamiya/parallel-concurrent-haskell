{-# Language TemplateHaskell #-}
{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}

import Text.Printf
import Data.Typeable
import Data.Binary
import GHC.Generics
import Control.Distributed.Process hiding (Message(..))
import Control.Distributed.Process.Serializable
import Control.Distributed.Process.Internal.Closure.TH

import DistribMain

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

main :: IO ()
main = distribMain (\_ -> master) Main.__remoteTable

