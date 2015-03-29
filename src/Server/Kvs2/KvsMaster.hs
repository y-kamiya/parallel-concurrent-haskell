{-# Language TemplateHaskell #-}

module Kvs2.KvsMaster where

import qualified Data.Map.Strict as M
import Control.Monad
import Control.Concurrent hiding (newChan)
import Control.Distributed.Process hiding (handleMessage)
import Control.Distributed.Process.Internal.Closure.TH

import Kvs2.KvsTypes
import Kvs2.KvsWorker

createDB :: [NodeId] -> Process Database
createDB peers = spawnLocal $ do
  pids <- forM peers $ \nid -> do
    say $ "spawn on: " ++ show nid
    spawn nid $ $(mkStaticClosure 'worker)

  let forward = forever $ do
        req <- expect
        let index = findWorker req $ length pids
        send (pids !! index) req

  if length pids == 0
    then worker
    else forward

findWorker :: Request -> Int -> Int
findWorker req workerNum = fromEnum c `mod` workerNum
  where c = head $ getKey req

getKey :: Request -> Key
getKey (ReqOp (Get k) _) = k
getKey (ReqOp (Set k _) _) = k

rcdata :: RemoteTable -> RemoteTable
rcdata = Kvs2.KvsWorker.__remoteTable

