import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.STM
import System.Environment
import Text.Printf
import qualified Data.Set as S
import qualified Data.Map as M

import Url

import Display
import TMVar
import Async
import TList
import TQueue

main :: IO ()
main = do
  [arg] <- getArgs
  case arg of
    "window_mvar" -> window_mvar
    "window_tvar" -> window_tvar
    "render_focus" -> render_focus
    "tmvar" -> tmvar
    "geturl" -> geturl
    "tlist" -> tlist
    "mlist" -> mlist

-- not work because both writeMList and readMList call takeMVar
mlist :: IO ()
mlist = do
  l <- TList.newMList
  _ <- forkIO $ replicateM_ 1 $ do
    print "before writeMList"
    TList.writeMList l "123"
    print "after writeMList"
  -- _ <-forkIO $ forever $ do
  --   s <- readMList l
  --   print $ "fork thread: " ++  s
  replicateM_ 1 $ do
    print "before readMList"
    s <- TList.readMList l
    print $ "main thread: " ++  s
  return ()

tlist :: IO ()
tlist = do
  l <- atomically TList.newTList
  _ <- forkIO $ replicateM_ 100 $ atomically $ TList.writeTList l "123"
  _ <-forkIO $ forever $ do
    s <- atomically $ TList.readTList l
    print $ "fork thread: " ++  s
  replicateM_ 10 $ do
    s <- atomically $ TList.readTList l
    print $ "main thread: " ++  s
  return ()

geturl :: IO ()
geturl = do
  let 
    download url = do
      r <- getURL url
      return (url, r)
  as <- mapM (async . download) sites
  (url, r) <- waitAny as
  printf "%s was first (%d bytes)" url (length r)

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
  m1 <- atomically $ TMVar.takeTMVar tmvar1
  m2 <- atomically $ TMVar.takeTMVar tmvar2
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
    Display.moveWindow disp (Window "A") (Desktop 1) (Desktop 2)
    putMVar checkThread1 "finished"

  checkThread2 <- newEmptyMVar
  _ <- forkIO $ do
    Display.moveWindow disp (Window "B") (Desktop 2) (Desktop 1)
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
    atomically $ Display.moveWindowSTM disp (Window "A") (Desktop 1) (Desktop 2)
    putMVar checkThread1 "finished"
    print "moved Window A to Desktop 2"

  checkThread2 <- newEmptyMVar
  _ <- forkIO $ do
    atomically $ Display.moveWindowSTM disp (Window "B") (Desktop 2) (Desktop 1)
    putMVar checkThread2 "finished"
    print "moved Window B to Desktop 1"

  takeMVar checkThread1
  takeMVar checkThread2

  desktop1 <- atomically $ readTVar tvar1
  desktop2 <- atomically $ readTVar tvar2
  print $ "Desktop 1 after: " ++ show desktop1
  print $ "Desktop 2 after: " ++ show desktop2
  return ()

render_focus :: IO ()
render_focus = do
  tvar1 <- newTVarIO $ S.fromList [Window "A"]
  tvar2 <- newTVarIO $ S.fromList [Window "B"]
  let disp = M.fromList [(Desktop 1, tvar1), (Desktop 2, tvar2)]

  focus <- newTVarIO $ Desktop 1
  _ <- forkIO $ forever $ do
    putStr "please input focus [1 or 2]: "
    input <- getLine
    atomically $ writeTVar focus $ Desktop (read input :: Int)
  
  Display.renderThread disp focus
  return ()


