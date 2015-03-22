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
    ["master", port ] -> do
      backend <- initializeBackend defaultHost port remoteTable
      startMaster backend master
    [ "slave" ] -> do                                               
      backend <- initializeBackend defaultHost defaultPort remoteTable   
      startSlave backend                                            
    [ "slave", port ] -> do                                         
      backend <- initializeBackend defaultHost port remoteTable
      startSlave backend                                            
    [ "slave", host, port ] -> do                                   
      backend <- initializeBackend host port remoteTable                 
      startSlave backend                                            

defaultHost = "localhost"
defaultPort = "44444"

