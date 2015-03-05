{-# LANGUAGE RecordWildCards #-}

module Chat where

import System.IO
import qualified Data.Map.Lazy as M
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Text.Printf

type ClientName = String

data Client = Client
  { clientName      ::  ClientName
  , clientHandle    ::  Handle
  , clientKicked    ::  TVar (Maybe String)
  , clientSendChan  ::  TChan Message
  }

data Message = Notice String
             | Tell ClientName String
             | Broadcast ClientName String
             | Command String

newClient :: ClientName -> Handle -> STM Client
newClient name handle = do
  c <- newTChan
  k <- newTVar Nothing
  return Client { clientName      = name
                , clientHandle    = handle
                , clientKicked    = k
                , clientSendChan  = c
                }

sendMessage :: Client -> Message -> STM ()
sendMessage Client{..} msg = writeTChan clientSendChan msg


data Server = Server { clients :: TVar (M.Map ClientName Client) }

newServer :: IO Server
newServer = do
  c <- newTVarIO M.empty
  return $ Server c

broadcast :: Server -> Message -> STM()
broadcast Server{..} msg = do
  clientmap <- readTVar clients
  mapM_ (\client -> sendMessage client msg) $ M.elems clientmap

checkAddClient :: Server -> ClientName -> Handle -> IO (Maybe Client)
checkAddClient server@Server{..} name handle = atomically $ do
  clientmap <- readTVar clients
  if M.member name clientmap
    then return Nothing
    else do
      client <- newClient name handle
      writeTVar clients $ M.insert name client clientmap
      broadcast server $ Notice $ name ++ " has connected"
      return $ Just client

removeClient :: Server -> ClientName -> IO ()
removeClient server@Server{..} name = atomically $ do
  modifyTVar' clients $ M.delete name
  broadcast server $ Notice $ name ++ " has disconnected"

talk :: Handle -> Server -> IO ()
talk handle server@Server{..} = do
  hSetNewlineMode handle universalNewlineMode
  hSetBuffering handle LineBuffering
  readName
  where
    readName = do
      hPutStrLn handle "What is your name ?"
      name <- hGetLine handle
      if null name
        then readName
        else mask $ \restore -> do
          ok <- checkAddClient server name handle
          case ok of
            Nothing -> restore $ do
              hPrintf handle "the name %s is in use, choose another\n" name
              readName
            Just client ->
              restore (runClient server client) `finally` removeClient server name

runClient :: Server -> Client -> IO ()
runClient server@Server{..} client@Client{..} = do
  race serverThread receiveThread
  return ()
  where
    receiveThread = forever $ do
      msg <- hGetLine clientHandle
      atomically $ sendMessage client $ Command msg

    serverThread = join $ atomically $ do
      k <- readTVar clientKicked
      case k of
        Just reason -> return $ hPutStrLn clientHandle $ "you have been kicked: " ++ reason
        Nothing -> do
          msg <- readTChan clientSendChan
          return $ do
            continue <- handleMessage server client msg
            when continue $ serverThread

handleMessage :: Server -> Client -> Message -> IO Bool
handleMessage server client@Client{..} message = 
  case message of
    Notice msg        -> output $ "*** " ++ msg
    Tell name msg     -> output $ "*" ++ name ++ "*: " ++ msg
    Broadcast name msg-> output $ "<" ++ name ++ ">: " ++ msg
    Command msg ->
      case words msg of
        ["/kick", who] -> do
          atomically $ kick server who clientName
          return True
        "/tell" : who : what -> do
          tell server client who $ unwords what
          return True
        ["/quit"] ->
          return False
        ('/':_):_ -> do
          hPutStrLn clientHandle $ "unrecognized command: " ++ msg
          return True
        _ -> do
          atomically $ broadcast server $ Broadcast clientName msg
          return True
  where
    output s = do
      hPutStrLn clientHandle s
      return True

sendToName :: Server -> ClientName -> Message -> STM Bool
sendToName server@Server{..} name msg = do
  clientmap <- readTVar clients
  case M.lookup name clientmap of
    Nothing -> return False
    Just client -> sendMessage client msg >> return True 

kick :: Server -> ClientName -> ClientName -> STM ()
kick server@Server{..} who by = do
  clientmap <- readTVar clients
  case M.lookup who clientmap of
    Nothing -> 
      void $ sendToName server by $ Notice (who ++ " is not connected")
    Just Client{..} -> do
      writeTVar clientKicked $ Just $ "by " ++ by
      void $ sendToName server by $ Notice ("you kicked " ++ who)

tell :: Server -> Client -> ClientName -> String -> IO ()
tell server@Server{..} Client{..} who s = do
  ok <- atomically $ sendToName server who $ Tell clientName s
  if ok
    then return ()
    else hPutStrLn clientHandle $ who ++ " is not connected"

