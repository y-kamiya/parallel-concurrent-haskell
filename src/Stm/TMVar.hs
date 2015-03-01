module TMVar where

import Control.Monad (void)
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TVar

newtype TMVar a = TMVar (TVar (Maybe a))

newEmptyTMVar :: STM (TMVar a)
newEmptyTMVar = do
  t <- newTVar Nothing
  return $ TMVar t

takeTMVar :: TMVar a -> STM a
takeTMVar (TMVar tvar) = do
  m <- readTVar tvar
  case m of
    Nothing -> retry
    Just a -> writeTVar tvar Nothing >> return a

putTMVar :: TMVar a -> a -> STM ()
putTMVar (TMVar tvar) a = do
  m <- readTVar tvar
  case m of
    Just _ -> retry
    Nothing -> void $ writeTVar tvar (Just a)



