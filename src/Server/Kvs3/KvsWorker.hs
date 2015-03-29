{-# Language TemplateHaskell #-}

module Kvs3.KvsWorker where

import qualified Data.Map.Strict as M
import Control.Monad
import Control.Concurrent hiding (newChan)
import Control.Distributed.Process hiding (handleMessage)
import Control.Distributed.Process.Internal.Closure.TH

import Kvs3.KvsTypes

handleMessage :: Request -> MVar (M.Map Key Value) -> Process ()

handleMessage (ReqOp (Get k) port) mvar = do
  map <- liftIO $ readMVar mvar
  let r = M.lookup k map
  sendChan port (ResGetResult r)

handleMessage (ReqOp (Set k v) port) mvar = do
  map <- liftIO $ takeMVar mvar
  let map' = M.insert k v map
  liftIO $ putMVar mvar map'
  sendChan port (ResSetResult $ (M.size map + 1) == M.size map')

worker :: Process ()
worker = do
  mvar <- liftIO $ newMVar M.empty
  forever $ do
    req <- expect
    handleMessage req mvar

$(remotable ['worker])

