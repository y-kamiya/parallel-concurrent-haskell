module DistribMain where

import System.Environment (getArgs)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable)

distribMain :: ([NodeId] -> Process ()) -> (RemoteTable -> RemoteTable) -> IO ()
distribMain master frtable = do
  args <- getArgs
  let remoteTable = frtable initRemoteTable

  case args of
    [] -> do
      backend <- initializeBackend defaultHost defaultPort remoteTable
      startMaster backend master
    ["master"] -> do
      backend <- initializeBackend defaultHost defaultPort remoteTable
      startMaster backend master

defaultHost = "localhost"
defaultPort = "44444"

