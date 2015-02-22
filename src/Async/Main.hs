import Control.Concurrent
import Network.HTTP
import System.Environment

-- data Async a = Async (MVar a)
--
-- async :: IO a -> IO (Async a)
-- async action = do
--   mvar <- newEmptyMvar
--   forkIO $ do
--     r <- action
--     putMVar mvar r
--   return (Async mvar)
--
-- wait :: Async a -> IO a
-- wait (Async mvar) = readMVar mvar

data Async a = Async (MVar (Either SomeException a))

sites :: [String]
sites = [ "http://www.google.com"
        , "http://www.bing.com"
        , "http://www.yahoo.com"
        , "http://www.wikipedia.com/wiki/Spade"
        , "http://www.wikipedia.com/wiki/Shovel"
        ]

async :: IO a -> IO (Async a)
async action = do
  mvar <- newEmptyMvar
  forkIO $ do
    e <- try action
    putMVar mvar e
  return (Async mvar)

wait :: Async a -> IO a
wait a = do
  e <- waitCatch a
  case e of
    Left exception -> throwIO exception
    Right r -> return r

waitCatch :: Async a -> IO (Either SomeException a)
watiCatch (Async mvar) = readMVar mvar

getURL :: String -> IO String
getURL url = simpleHTTP (getRequest url) >>= getResponseBody

main :: IO ()
main = do
  [arg] <- getArgs
  case arg of
    "1" -> geturl1
    "2" -> geturl2
    "5" -> geturl5
    "6" -> geturl6
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

geturl5 :: IO ()
geturl5 = do
  m <- newEmptyMVar
  let 
    download url = do
      r <- getURL url
      putMVar m (url, r)
  mapM_ (forkIO . download) sites

  (url, r) <- takeMVar m
  printf "%s was first (%d bytes)\n" url (length r)
  replicateM_ (length sites - 1) $ takeMVar m

waitAny :: [Async a] -> IO a
waitAny as = do
  mvar <- newEmptyMVar
  let 
    forkWait a = forkIO $ do
      r <- try $ wait a
      putMVar mvar r
  mapM_ forkWait as
  wait $ Async mvar

geturl6 :: IO ()
geturl6 = do
  let 
    download url = do
      r <- getURL url
      return (url, r)
  as <- mapM_ (async . download) sites
  (url, r) <- waitAny as
  printf "%s was first (%d bytes)" url r
  mapM_ wait as
  
