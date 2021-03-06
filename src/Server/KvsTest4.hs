{-# LANGUAGE OverloadedStrings #-}

module KvsTest4 where

import Control.Distributed.Process
import Control.Monad.IO.Class
import Control.Monad
import System.IO

import DistribMain

import Kvs4.KvsTypes
import Kvs4.KvsMaster  (createDB, rcdata)

defaultMain = distribMain master rcdata

master :: [NodeId] -> Process ()
master peers = do
  db <- createDB peers

  f <- liftIO $ readFile "/etc/protocols"
  let ws = words f

  zipWithM_ (set db) ws (tail ws)

  get db "module" >>= liftIO . print
  get db "xxxx"   >>= liftIO . print

  forever $ do
    l <- liftIO $ do putStr "key: "; hFlush stdout; getLine
    when (not (null l)) $ do
      r <- get db l
      liftIO $ putStrLn ("response: " ++ show r)

  return ()

set :: Database -> Key -> Value -> Process Bool
set db k v = do
  (sp, rp) <- newChan
  send db $ ReqOp (Set k v) sp
  ResSetResult r <- receiveChan rp
  return r

get :: Database -> Key -> Process (Maybe Value)
get db k = do
  (sp, rp) <- newChan
  send db $ ReqOp (Get k) sp
  ResGetResult r <- receiveChan rp
  return r

