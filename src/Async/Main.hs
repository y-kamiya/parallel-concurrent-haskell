import Control.Concurrent
import Network.HTTP
import System.Environment

data Async a = Async (MVar a)

async :: IO a -> IO (Async a)
async action = do
  mvar <- newEmptyMvar
  forkIO $ do
    r <- action
    putMVar mvar r
  return (Async mvar)

wait :: Async a -> IO a
wait (Async mvar) = readMVar mvar

getURL :: String -> IO String
getURL url = simpleHTTP (getRequest url) >>= getResponseBody

main :: IO ()
main = do
  [arg] <- getArgs
  case arg of
    "1" -> geturl1
    "2" -> geturl2
    _ -> print "no such arguments"

geturl1 :: IO ()
geturl1 = do
  m1 <- newEmptyMVar
  m2 <- newEmptyMVar
  forkIO $ do
    r1 <- getURL "http://hackage.haskell.org/package/HTTP-4000.0.5/docs/Network-HTTP.html"
    putMVar m1 r1
  forkIO $ do
    r2 <- getURL "http://hackage.haskell.org/package/HTTP-4000.0.6/docs/Network-HTTP.html"
    putMVar m2 r2
  r1' <- takeMVar m1
  r2' <- takeMVar m2
  print (length r1', length r2')

geturl2 :: IO ()
geturl2 = do
  a1 <- async $ getURL "http://hackage.haskell.org/package/HTTP-4000.0.5/docs/Network-HTTP.html"
  a2 <- async $ getURL "http://hackage.haskell.org/package/HTTP-4000.0.6/docs/Network-HTTP.html"
  r1 <- wait a1
  r2 <- wait a2
  print (length r1, length r2)
