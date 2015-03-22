{-# LANGUAGE RecordWildCards #-}
{-# Language TemplateHaskell #-}
{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}

module DistribChat where

import System.IO
import Network
import Text.Printf
import Data.Typeable
import Data.Binary
import GHC.Generics
import qualified Data.Map.Lazy as M
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Distributed.Process hiding (Message(..), proxy, mask, finally, handleMessage)
import Control.Distributed.Process.Internal.Closure.TH

import DistribMain

port :: Int
port = 44444

type ClientName = String

data Client
  = ClientLocal LocalClient
  | ClientRemote RemoteClient

data LocalClient = LocalClient
      { localName       :: ClientName
      , clientHandle    :: Handle
      , clientKicked    :: TVar (Maybe String)
      , clientSendChan  :: TChan Message
      }

data RemoteClient = RemoteClient
      { remoteName  :: ClientName
      , clientHome  :: ProcessId
      }

clientName :: Client -> ClientName
clientName (ClientLocal c) = localName c
clientName (ClientRemote c) = remoteName c

newLocalClient :: ClientName -> Handle -> STM LocalClient
newLocalClient name handle = do
  c <- newTChan
  k <- newTVar Nothing
  return LocalClient { localName = name 
                     , clientHandle = handle
                     , clientSendChan = c
                     , clientKicked = k
                     }

data Message = Notice String
             | Tell ClientName String
             | Broadcast ClientName String
             | Command String
 deriving (Typeable, Generic)

instance Binary Message

data PMessage
  = MsgServers            [ProcessId]
  | MsgSend               ClientName Message
  | MsgBroadcast          Message
  | MsgKick               ClientName ClientName
  | MsgNewClient          ClientName ProcessId
  | MsgClientDisconnected ClientName ProcessId
  deriving (Typeable, Generic)

instance Binary PMessage

data Server = Server
  { clients   :: TVar (M.Map ClientName Client)
  , proxychan :: TChan (Process ())
  , servers   :: TVar [ProcessId]
  , spid      :: ProcessId
  }

newServer :: [ProcessId] -> Process Server
newServer pids = do
  pid <- getSelfPid
  liftIO $ do
    s <- newTVarIO pids
    c <- newTVarIO $ M.empty
    o <- newTChanIO
    return Server { clients = c, servers = s, proxychan = o, spid = pid }

sendLocal :: LocalClient -> Message -> STM ()
sendLocal LocalClient{..} msg = writeTChan clientSendChan msg

sendRemote :: Server -> ProcessId -> PMessage -> STM ()
sendRemote Server{..} pid pmsg = writeTChan proxychan (send pid pmsg)

sendMessage :: Server -> Client -> Message -> STM ()
sendMessage server (ClientLocal client) msg = sendLocal client msg
sendMessage server (ClientRemote client) msg = sendRemote server (clientHome client) (MsgSend (remoteName client) msg)

sendToName :: Server -> ClientName -> Message -> STM Bool
sendToName server@Server{..} name msg = do
  clientmap <- readTVar clients
  case M.lookup name clientmap of
    Nothing -> return False
    Just client -> sendMessage server client msg >> return True

sendRemoteAll :: Server -> PMessage -> STM ()
sendRemoteAll server@Server{..} pmsg = do
  pids <- readTVar servers
  mapM_ (\pid -> sendRemote server pid pmsg) pids

broadcastLocal :: Server -> Message -> STM ()
broadcastLocal server@Server{..} msg = do
  clientmap <- readTVar clients
  mapM_ sendIfLocal $ M.elems clientmap
  where
    sendIfLocal (ClientLocal c) = sendLocal c msg
    sendIfLocal (ClientRemote _) = return ()

broadcast :: Server -> Message -> STM ()
broadcast server msg = do
  sendRemoteAll server (MsgBroadcast msg)
  broadcastLocal server msg

handleRemoteMessage :: Server -> PMessage -> Process ()
handleRemoteMessage server@Server{..} msg = liftIO $ atomically $
  case msg of
    MsgServers pids   -> writeTVar servers $ filter (/= spid) pids
    MsgSend name msg  -> void $ sendToName server name msg
    MsgBroadcast msg  -> broadcastLocal server msg
    MsgKick who by    -> kick server who by

    MsgNewClient name pid -> do
      ok <- checkAddClient server $ ClientRemote (RemoteClient name pid)
      when (not ok) $
        sendRemote server pid (MsgKick name "SYSTEM")

    MsgClientDisconnected name pid -> do
      clientmap <- readTVar clients
      case M.lookup name clientmap of
        Nothing -> return ()
        Just (ClientRemote (RemoteClient _ pid'))
          | pid == pid' -> removeClient server name 
        Just _ -> return ()

-- copy from original chat
checkAddClient :: Server -> Client -> STM Bool
checkAddClient server@Server{..} client = do
  clientmap <- readTVar clients
  let name = clientName client
  if M.member name clientmap
    then return False
    else do
      writeTVar clients $ M.insert name client clientmap
      broadcastLocal server $ Notice $ name ++ " has connected"
      return True

removeClient :: Server -> ClientName -> STM ()
removeClient server@Server{..} name = do
  modifyTVar' clients $ M.delete name
  broadcastLocal server $ Notice $ name ++ " has disconnected"

removeClientAndBroadcast :: Server -> ClientName -> ProcessId -> STM ()
removeClientAndBroadcast server name pid = do
  removeClient server name
  sendRemoteAll server $ MsgClientDisconnected name pid

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
          client <- atomically $ newLocalClient name handle 
          ok <- atomically $ checkAddClient server $ ClientLocal client
          case ok of
            False -> restore $ do
              hPrintf handle "the name %s is in use, choose another\n" name
              readName
            True -> do
              atomically $ sendRemoteAll server $ MsgNewClient name spid
              restore (runClient server client) `finally` atomically (removeClientAndBroadcast server name spid)

runClient :: Server -> LocalClient -> IO ()
runClient server@Server{..} client@LocalClient{..} = do
  race serverThread receiveThread
  return ()
  where
    receiveThread = forever $ do
      msg <- hGetLine clientHandle
      atomically $ sendMessage server (ClientLocal client) $ Command msg

    serverThread = join $ atomically $ do
      k <- readTVar clientKicked
      case k of
        Just reason -> return $ hPutStrLn clientHandle $ "you have been kicked: " ++ reason
        Nothing -> do
          msg <- readTChan clientSendChan
          return $ do
            continue <- handleMessage server client msg
            when continue $ serverThread

kick :: Server -> ClientName -> ClientName -> STM ()
kick server@Server{..} who by = do
  clientmap <- readTVar clients
  case M.lookup who clientmap of
    Nothing -> 
      void $ sendToName server by $ Notice (who ++ " is not connected")
    Just (ClientLocal LocalClient{..}) -> do
      writeTVar clientKicked $ Just $ "by " ++ by
      void $ sendToName server by $ Notice ("you kicked " ++ who)
    Just (ClientRemote RemoteClient{..}) -> return ()

tell :: Server -> LocalClient -> ClientName -> String -> IO ()
tell server@Server{..} LocalClient{..} who s = do
  ok <- atomically $ sendToName server who $ Tell localName s
  if ok
    then return ()
    else hPutStrLn clientHandle $ who ++ " is not connected"

handleMessage :: Server -> LocalClient -> Message -> IO Bool
handleMessage server client@LocalClient{..} message = 
  case message of
    Notice msg        -> output $ "*** " ++ msg
    Tell name msg     -> output $ "*" ++ name ++ "*: " ++ msg
    Broadcast name msg-> output $ "<" ++ name ++ ">: " ++ msg
    Command msg ->
      case words msg of
        ["/kick", who] -> atomically $ do
          kick server who localName
          sendRemoteAll server $ MsgKick who localName
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
          atomically $ broadcast server $ Broadcast localName msg
          return True
  where
    output s = do
      hPutStrLn clientHandle s
      return True

socketListener :: Server -> Int -> IO ()
socketListener server port = withSocketsDo $ do
  sock <- listenOn $ PortNumber (fromIntegral port)
  printf "Listening on port %d\n" port
  forever $ do
    (handle, host, port) <- accept sock
    printf "Accepted connection from %s: %s\n" host (show port)
    forkFinally (talk handle server) (\_ -> hClose handle)

proxy :: Server -> Process ()
proxy Server{..} = forever $ join $ liftIO $ atomically $ readTChan proxychan

chatServer :: Int -> Process ()
chatServer port = do
  server <- newServer []
  liftIO $ forkIO (socketListener server port)
  spawnLocal $ proxy server
  forever $ do
    m <- expect
    handleRemoteMessage server m

$(remotable ['DistribChat.chatServer])

master :: [NodeId] -> Process ()
master peers = do
  let run nid port = do
        say $ printf "spawning on %s" (show nid)
        spawn nid $ $(mkClosure 'chatServer) port

  pids <- zipWithM run peers [port+1..]
  mypid <- getSelfPid
  let all_pids = mypid : pids
  mapM_ (\pid -> send pid $ MsgServers all_pids) all_pids

  chatServer port

defaultMain :: IO ()
defaultMain = distribMain master DistribChat.__remoteTable
