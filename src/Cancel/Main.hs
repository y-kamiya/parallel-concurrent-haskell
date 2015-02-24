import Control.Monad
import Control.Concurrent
import Control.Exception
import Network.HTTP
import System.IO
import System.Environment
import System.TimeIt
import Text.Printf
import Data.Either (rights)

data Async a = Async ThreadId (MVar (Either SomeException a))

sites :: [String]
sites = [ "http://hackage.haskell.org/package/HTTP"
        , "http://hackage.haskell.org/package/async"
        , "http://hackage.haskell.org/package/base-4.7.0.2/docs/Control-Concurrent.html"
        ]

getURL :: String -> IO String
getURL url = simpleHTTP (getRequest url) >>= getResponseBody

timeDownload :: String -> IO ()
timeDownload url = do
  (time, page) <- timeItT $ getURL url
  printf "downloaded: %s (%d bytes, %.2fs)\n" url (length page) time

async :: IO a -> IO (Async a)
async action = do
  mvar <- newEmptyMVar
  tid <- forkIO $ do
    e <- try action
    putMVar mvar e
  return $ Async tid mvar

wait :: Async a -> IO a
wait a = do
  e <- waitCatch a
  case e of
    Left exception -> throwIO exception
    Right r -> return r

waitCatch :: Async a -> IO (Either SomeException a)
waitCatch (Async _ mvar) = readMVar mvar

cancel :: Async a -> IO ()
cancel (Async tid _) = throwTo tid ThreadKilled

main :: IO ()
main = do
  [arg] <- getArgs
  case arg of
    "3" -> geturl3
    "7" -> geturl7
    _ -> print "no such arguments"

geturl3 :: IO ()
geturl3 = do
  as <- mapM (async . timeDownload) sites
  mapM_ wait as

geturl7 :: IO ()
geturl7 = do
  as <- mapM (async . timeDownload) sites
  forkIO $ do
    hSetBuffering stdin NoBuffering
    forever $ do
      c <- getChar
      when (c == 'q') $ mapM_ cancel as

  rs <- mapM waitCatch as
  printf "%d/%d succeeded\n" (length (rights rs)) (length rs)

timeout :: Int -> IO a -> IO (Maybe a)
timeout t m 
  | t < 0 = fmap Just m
  | t == 0 = return Nothing
  | otherwise = do
    pid <- myThreadId
    u <- newUnique
    let ex = Timeout u
    handleJust
      (\e -> if e == ex then Just () else Nothing)
      (\_ -> return Nothing)
      (bracket 
        (forkIO $ do threadDelay t; throwTo pid ex)
        (\tid -> throwTo tid ThreadKilled)
        (\_ -> fmap Just m))

