module Async where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception

data Async a = Async ThreadId (TMVar (Either SomeException a))

async :: IO a -> IO (Async a)
async action = do
  tmvar <- newEmptyTMVarIO
  tid <- forkFinally action $ atomically . putTMVar tmvar
  return $ Async tid tmvar

waitCatchSTM :: Async a -> STM (Either SomeException a)
waitCatchSTM (Async _ tmvar) = readTMVar tmvar

waitSTM :: Async a -> STM a
waitSTM a = do
  e <- waitCatchSTM a
  case e of
    Left err -> throwSTM err
    Right r -> return r

waitEither :: Async a -> Async b -> IO (Either a b)
waitEither a b = atomically $
  fmap Left (waitSTM a) `orElse` fmap Right (waitSTM b)

waitBoth :: Async a -> Async b -> IO (a, b)
waitBoth a b = atomically $ do
  r1 <- waitSTM a `orElse` (do waitSTM b; retry)
  r2 <- waitSTM b
  return (r1, r2)

waitAny :: [Async a] -> IO a
waitAny as = atomically $ foldr orElse retry $ map waitSTM as

withAsync :: IO a -> (Async a -> IO b) -> IO b
withAsync io = bracket (async io) cancel
  where
    cancel :: Async a -> IO ()
    cancel (Async tid _) = throwTo tid ThreadKilled

concurrently :: IO a -> IO b -> IO (a, b)
concurrently ioa iob = 
  withAsync ioa $ \a ->
  withAsync iob $ \b -> 
    waitBoth a b

race :: IO a -> IO b -> IO (Either a b)
race ioa iob = 
  withAsync ioa $ \a ->
  withAsync iob $ \b -> 
    waitEither a b

timeout :: Int -> IO a -> IO (Maybe a)
timeout n io
  | n < 0 = fmap Just io
  | n == 0 = return Nothing
  | otherwise = do
    r <- race (threadDelay n) io
    case r of
      Left _ -> return Nothing
      Right a -> return $ Just a
