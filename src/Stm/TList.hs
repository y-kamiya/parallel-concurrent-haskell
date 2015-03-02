module TList where

import Control.Concurrent.STM

newtype TList a = TList (TVar [a])

newTList :: STM (TList a)
newTList = do
  v <- newTVar []
  return $ TList []

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

  
