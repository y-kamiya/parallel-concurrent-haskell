module TChan where

import Control.Monad.STM
import Control.Concurrent.STM.TVar

data TChan a = TChan (TVar (TVarList a))
                     (TVar (TVarList a))

type TVarList a = TVar (TList a)

data TList a = TNil | TCons a (TVarList a)

newTChan :: STM (TChan a)
newTChan = do
  hole <- newTVar TNil
  read <- newTVar hole
  write <- newTVar hole
  return $ TChan read write

readTChan :: TChan a -> STM a
readTChan (TChan read _) = do
  listHead <- readTVar read
  tList <- readTVar listHead
  case tList of
    TNil -> retry
    TCons a tail -> do
      writeTVar read tail
      return a

writeTChan :: TChan a -> a -> STM ()
writeTChan (TChan _ write) a = do
  newEnd <- newTVar TNil
  listEnd <- readTVar write
  writeTVar write newEnd
  writeTVar listEnd $ TCons a newEnd

unGetTChan :: TChan a -> a -> STM ()
unGetTChan (TChan read _) a = do
  newHead <- newTVar TNil
  listHead <- readTVar read
  writeTVar read newHead
  writeTVar newHead $ TCons a listHead

isEmptyTChan :: TChan a -> STM Bool
isEmptyTChan (TChan read _) = do
  listHead <- readTVar read
  head <- readTVar listHead
  case head of
    TNil -> return True
    _ -> return False

readEitherTChan :: TChan a -> TChan b -> STM (Either a b)
readEitherTChan ta tb = fmap Left (readTChan ta) `orElse` fmap Right (readTChan tb)


