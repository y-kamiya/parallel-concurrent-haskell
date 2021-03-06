import System.IO
import System.Environment (getArgs, withArgs)
import Text.Printf
import Network
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Distributed.Process hiding (Message(..))
import Control.Distributed.Process.Backend.SimpleLocalnet

import Chat
import qualified DistribChat as DistribChat
import qualified DistribChatNoSlave as DistribChatNoSlave
import qualified KvsTest1 as KvsTest1
import qualified KvsTest2 as KvsTest2
import qualified KvsTest3 as KvsTest3
import qualified KvsTest4 as KvsTest4

port :: Int
port = 44444

main :: IO ()
main = do
  (arg:args) <- getArgs
  case arg of
    "trivial" -> trivial
    "simple" -> simple
    "chat" -> chat
    "dchat" -> withArgs args $ DistribChat.defaultMain
    "noslave" -> DistribChatNoSlave.defaultMain args
    "kvstest1" -> withArgs args $ KvsTest1.defaultMain
    "kvstest2" -> withArgs args $ KvsTest2.defaultMain
    "kvstest3" -> withArgs args $ KvsTest3.defaultMain
    "kvstest4" -> withArgs args $ KvsTest4.defaultMain
    _ -> do
      print $ "arg: " ++ arg
      print "not match defined arg. start trivial server"
      trivial

chat :: IO ()
chat = do
  server <- newServer
  sock <- listenOn $ PortNumber $ fromIntegral port
  printf "Listening on port %d\n" port
  forever $ do
    (handle, host, port) <- accept sock
    printf "Accepted connection from %s: %s\n" host (show port)
    forkFinally (talk handle server) (\_ -> hClose handle)

trivial :: IO ()
trivial = withSocketsDo $ do
  sock <- listenOn $ PortNumber $ fromIntegral port
  printf "Listening on port %d\n" port
  forever $ do
    (handle, host, port) <- accept sock
    printf "Accepted connection from %s: %s\n" host (show port)
    forkFinally (talk handle) (\_ -> hClose handle)
  where
    talk :: Handle -> IO ()
    talk h = do
      hSetBuffering h LineBuffering
      loop
      where
        loop = do
          line <- hGetLine h
          if line == "quit"
            then hPutStrLn h "finished doubling service"
            else do
              hPutStrLn h $ show $ 2 * (read line :: Integer)
              loop

simple :: IO ()
simple = withSocketsDo $ do
  sock <- listenOn $ PortNumber $ fromIntegral port
  printf "Listening on port %d\n" port
  factor <- atomically $ newTVar 2
  forever $ do
    (handle, host, port) <- accept sock
    printf "Accepted connection from %s: %s\n" host (show port)
    forkFinally (talk handle factor) (\_ -> hClose handle)
  where
    talk :: Handle -> TVar Integer -> IO ()
    talk h factor = do
      hSetBuffering h LineBuffering
      c <- atomically newTChan
      race (server h factor c) (receive h c)
      return ()

    receive :: Handle -> TChan String -> IO ()
    receive h c = forever $ do
      line <- hGetLine h
      atomically $ writeTChan c line

    server :: Handle -> TVar Integer -> TChan String -> IO ()
    server h factor c = do
      f <- atomically $ readTVar factor
      hPrintf h "Current factor: %d\n" f
      loop f
      where
        loop f = do
          action <- atomically $ do
            f' <- readTVar factor
            if f /= f'
               then return $ newfactor f'
               else do
                 l <- readTChan c
                 return $ command f l
          action

        newfactor f = do
          hPrintf h "new factor: %d\n" f
          loop f

        command f s
         = case s of
             "quit" -> hPutStrLn h "finished doubling service"
             '*':s -> do
               atomically $ writeTVar factor (read s :: Integer)
               loop f
             line -> do
               hPutStrLn h $ show $ f * (read line :: Integer)
               loop f
