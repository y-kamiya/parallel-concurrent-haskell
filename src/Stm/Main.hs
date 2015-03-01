import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Control.Monad.IO.Class
import qualified Data.Set as S
import qualified Data.Map as M
import System.Environment

import TMVar

main :: IO ()
main = do
  [arg] <- getArgs
  case arg of
    "window_mvar" -> window_mvar
    "window_tvar" -> window_tvar
    "tmvar" -> tmvar

tmvar :: IO ()
tmvar = do
  tmvar1 <- atomically $ newEmptyTMVar
  tmvar2 <- atomically $ newEmptyTMVar
  forkIO $ do
    print "put 1 to tmvar1"
    atomically $ putTMVar tmvar1 1
  forkIO $ do
    print "put 2 to tmvar2"
    atomically $ putTMVar tmvar2 2
  m1 <- atomically $ takeTMVar tmvar1
  m2 <- atomically $ takeTMVar tmvar2
  print m2
  print m1

window_mvar :: IO ()
window_mvar = do
  mvar1 <- newMVar $ S.fromList [Window "A"]
  mvar2 <- newMVar $ S.fromList [Window "B"]
  let disp = M.fromList [(Desktop 1, mvar1), (Desktop 2, mvar2)]

  desktop1 <- readMVar mvar1
  desktop2 <- readMVar mvar2
  print $ "Desktop 1 before: " ++ show desktop1
  print $ "Desktop 2 before: " ++ show desktop2

  checkThread1 <- newEmptyMVar
  _ <- forkIO $ do
    moveWindow disp (Window "A") (Desktop 1) (Desktop 2)
    putMVar checkThread1 "finished"

  checkThread2 <- newEmptyMVar
  _ <- forkIO $ do
    moveWindow disp (Window "B") (Desktop 2) (Desktop 1)
    putMVar checkThread2 "finished"

  takeMVar checkThread1
  takeMVar checkThread2

  desktop1' <- takeMVar mvar1
  desktop2' <- takeMVar mvar2
  print $ "Desktop 1 after: " ++ show desktop1'
  print $ "Desktop 2 after: " ++ show desktop2'
  return ()

window_tvar :: IO ()
window_tvar = do
  tvar1 <- newTVarIO $ S.fromList [Window "A"]
  tvar2 <- newTVarIO $ S.fromList [Window "B"]
  let disp = M.fromList [(Desktop 1, tvar1), (Desktop 2, tvar2)]

  checkThread1 <- newEmptyMVar
  _ <- forkIO $ do
    atomically $ moveWindowSTM disp (Window "A") (Desktop 1) (Desktop 2)
    putMVar checkThread1 "finished"
    print "moved Window A to Desktop 2"

  checkThread2 <- newEmptyMVar
  _ <- forkIO $ do
    atomically $ moveWindowSTM disp (Window "B") (Desktop 2) (Desktop 1)
    putMVar checkThread2 "finished"
    print "moved Window B to Desktop 1"

  takeMVar checkThread1
  takeMVar checkThread2

  desktop1 <- atomically $ readTVar tvar1
  desktop2 <- atomically $ readTVar tvar2
  print $ "Desktop 1 after: " ++ show desktop1
  print $ "Desktop 2 after: " ++ show desktop2
  return ()

newtype Window = Window String deriving (Eq, Show, Ord)
            
newtype Desktop = Desktop Int deriving (Eq, Show, Ord)

type DisplayMVar = M.Map Desktop (MVar (S.Set Window))

moveWindow :: DisplayMVar -> Window -> Desktop -> Desktop -> IO ()
moveWindow disp w da db = do
  wa <- takeMVar ma
  print "lock Desktop passed by third args"
  wb <- takeMVar mb
  putMVar ma $ S.delete w wa
  putMVar mb $ S.insert w wb
  where
    ma = disp M.! da
    mb = disp M.! db

type DisplayTVar = M.Map Desktop (TVar (S.Set Window))

moveWindowSTM :: DisplayTVar -> Window -> Desktop -> Desktop -> STM ()
moveWindowSTM disp w da db = do
  wa <- readTVar ma
  wb <- readTVar mb
  writeTVar ma $ S.delete w wa
  writeTVar mb $ S.insert w wb
  where
    ma = disp M.! da
    mb = disp M.! db

