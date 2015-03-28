{-# LANGUAGE OverloadedStrings #-}

module KvsTest where

import Control.Distributed.Process
import Control.Monad.IO.Class
import Control.Monad
import System.IO

import DistribMain

import KvsMaster  (Database, createDB, get, set, rcdata)

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
