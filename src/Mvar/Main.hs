import Control.Concurrent
import Control.Monad
import System.IO
import System.Environment
import Text.Printf

import Logger

main :: IO ()
main = do
  [arg] <- getArgs
  case arg of
    "ab" -> outputAB
    "reminder" -> reminder
    "reminder2" -> reminder2
    "mvar" -> mvar
    "logger" -> logger
    _ -> print "no such argument"

logger :: IO ()
logger = do
  l <- initLogger
  logMessage l "log first message"
  logMessage l "log second message"
  logStop l

mvar :: IO ()
mvar = do
  m <- newEmptyMVar 
  forkIO $ do
    putMVar m 'x'
    putMVar m 'y'
  r <- takeMVar m
  print r
  r <- takeMVar m
  print r

outputAB :: IO ()
outputAB = do
  hSetBuffering stdout NoBuffering
  forkIO (replicateM_ 1000 (putChar 'A'))
  replicateM_ 1000 (putChar 'B')
  
setReminder :: String -> IO ()
setReminder s = do
  let t = read s :: Int
  printf "Remind you %d seconds\n" t
  threadDelay $ t * 10^6
  printf "%d seconds is up!\n" t

reminder :: IO ()
reminder = do
  hSetBuffering stdout NoBuffering
  forever $ do
     s <- getLine
     forkIO $ setReminder s

reminder2 :: IO ()
reminder2 = do
  hSetBuffering stdout NoBuffering
  loop
  where
    loop = do
      s <- getLine
      if s == "exit"
         then return ()
         else do forkIO $ setReminder s
                 loop


