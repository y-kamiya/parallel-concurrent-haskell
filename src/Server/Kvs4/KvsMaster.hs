{-# Language TemplateHaskell #-}

module Kvs4.KvsMaster where

import qualified Data.Map.Strict as M
import Text.Printf
import Control.Monad
import Control.Concurrent hiding (newChan)
import Control.Distributed.Process hiding (handleMessage)
import Control.Distributed.Process.Internal.Closure.TH

import Kvs4.KvsTypes
import Kvs4.KvsWorker

createDB :: [NodeId] -> Process Database
createDB peers = spawnLocal $ do
  pids <- forM peers $ \nid -> do
    say $ "spawn on: " ++ show nid
    spawn nid $ $(mkStaticClosure 'worker)

  _ <- mapM_ monitor pids

  let groups = groupPair pids

  if null pids
    then worker
    else waitMessage groups

groupPair :: [a] -> [[a]]
groupPair [] = []
groupPair [x] = []
groupPair (x:y:xs) = [x,y] : groupPair xs

waitMessage :: [[ProcessId]] -> Process ()
waitMessage groups = receiveWait 
  [ match $ handleNotification groups
  , match $ handleRequest groups
  , matchAny $ \msg -> do
      say $ printf "received unknownMessage: %s" (show msg)
  ]

handleNotification :: [[ProcessId]] -> ProcessMonitorNotification -> Process ()
handleNotification groups (ProcessMonitorNotification ref pid reason) = do
  say $ printf "died worker %s by %s" (show pid) (show reason)
  unmonitor ref
  waitMessage $ map (filter (/= pid)) groups

handleRequest :: [[ProcessId]] -> Request -> Process ()
handleRequest groups req = do
  let index = findWorker req $ length groups
      group = groups !! index
  forM_ group $ \pid -> send pid req
  waitMessage groups

findWorker :: Request -> Int -> Int
findWorker req workerNum = fromEnum c `mod` workerNum
  where c = head $ getKey req

getKey :: Request -> Key
getKey (ReqOp (Get k) _) = k
getKey (ReqOp (Set k _) _) = k

rcdata :: RemoteTable -> RemoteTable
rcdata = Kvs4.KvsWorker.__remoteTable

