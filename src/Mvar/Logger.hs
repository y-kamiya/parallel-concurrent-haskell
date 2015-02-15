module Logger (
  initLogger
, logMessage
, logStop
) where 


import Control.Concurrent

data Logger = Logger (MVar LogCommand)

data LogCommand = Message String | Stop (MVar ())

initLogger :: IO Logger
initLogger = do
  m <- newEmptyMVar
  let l = Logger m
  forkIO (logger l)
  return l

logger :: Logger -> IO ()
logger (Logger m) = loop
  where
    loop = do
      cmd <- takeMVar m
      case cmd of
        Message s -> do 
          print s
          loop
        Stop s -> do 
          print "stop logger"
          putMVar s ()

logMessage :: Logger -> String -> IO ()
logMessage (Logger m) msg = putMVar m $ Message msg

logStop :: Logger -> IO ()
logStop (Logger m) = do
  m' <- newEmptyMVar
  putMVar m $ Stop m'
  takeMVar m'

