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

waitAny :: [Async a] -> IO a
waitAny as = atomically $ foldr orElse retry $ map waitSTM as

