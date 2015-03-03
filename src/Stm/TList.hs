module TList where

import Control.Concurrent
import Control.Concurrent.STM

newtype TList a = TList (TVar [a])

newTList :: STM (TList a)
newTList = do
  v <- newTVar []
  return $ TList v

writeTList :: TList a -> a -> STM ()
writeTList (TList v) a = do
  list <- readTVar v
  writeTVar v $ list ++ [a]

readTList :: TList a -> STM a
readTList (TList v) = do
  list <- readTVar v
  case list of
    [] -> retry
    (x:xs) -> writeTVar v xs >> return x

  
newtype MList a = MList (MVar [a])

newMList :: IO (MList a)
newMList = do
  v <- newMVar []
  return $ MList v

writeMList :: MList a -> a -> IO ()
writeMList (MList v) a = do
  list <- readMVar' v
  putMVar v $ list ++ [a]

readMList :: MList a -> IO a
readMList m@(MList v) = do
  list <- readMVar' v
  case list of
    [] -> readMList m
    (x:xs) -> putMVar v xs >> return x

readMVar' :: MVar a -> IO a
readMVar' m = do
  a <- takeMVar m
  putMVar m a
  return a
  

