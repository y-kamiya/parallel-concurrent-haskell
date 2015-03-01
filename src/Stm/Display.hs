module Display where

import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Set as S
import qualified Data.Map as M

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

render :: S.Set Window -> IO ()
render wins = print wins

type UserFocus = TVar Desktop

getWindows :: DisplayTVar -> UserFocus -> STM (S.Set Window)
getWindows disp focus = do
  desktop <- readTVar focus
  readTVar $ disp M.! desktop

renderThread :: DisplayTVar-> UserFocus -> IO ()
renderThread disp focus = do
  wins <- atomically $ getWindows disp focus
  loop wins
  where
    loop wins = do
      render wins
      next <- atomically $ do
        wins' <- getWindows disp focus
        if wins == wins'
          then retry
          else return wins'
      loop next

