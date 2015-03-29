{-# Language TemplateHaskell #-}

module Kvs3.KvsMaster where

import qualified Data.Map.Strict as M
import Text.Printf
import Control.Monad
import Control.Concurrent hiding (newChan)
import Control.Distributed.Process hiding (handleMessage)
import Control.Distributed.Process.Internal.Closure.TH

import Kvs3.KvsTypes
import Kvs3.KvsWorker

createDB :: [NodeId] -> Process Database
createDB peers = spawnLocal $ do
  pids <- forM peers $ \nid -> do
    say $ "spawn on: " ++ show nid
    spawn nid $ $(mkStaticClosure 'worker)

  _ <- mapM_ monitor pids

  let handle = forever $ do
        receiveWait 
          [ match handleNotification
          , match $ handleRequest pids
          , matchAny $ \msg -> do
              say $ printf "received unknownMessage: %s" (show msg)
          ]

  if null pids
    then worker
    else handle

handleNotification :: ProcessMonitorNotification -> Process ()
handleNotification (ProcessMonitorNotification ref pid reason) = do
  say $ printf "died worker %s by %s" (show pid) (show reason)
  unmonitor ref

handleRequest :: [ProcessId] -> Request -> Process ()
handleRequest pids req = do
  let index = findWorker req $ length pids
  send (pids !! index) req

findWorker :: Request -> Int -> Int
findWorker req workerNum = fromEnum c `mod` workerNum
  where c = head $ getKey req

getKey :: Request -> Key
getKey (ReqOp (Get k) _) = k
getKey (ReqOp (Set k _) _) = k

rcdata :: RemoteTable -> RemoteTable
rcdata = Kvs3.KvsWorker.__remoteTable

