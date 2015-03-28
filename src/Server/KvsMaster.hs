{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}

module KvsMaster where

import qualified Data.Map.Strict as M

import GHC.Generics
import Data.Typeable
import Data.Binary
import Control.Monad (forever)
import Control.Concurrent hiding (newChan)
import Control.Distributed.Process hiding (handleMessage)
import Control.Distributed.Process.Serializable

type Key = String
type Value = String

type Database = ProcessId

data Request = ReqOp Command (SendPort Response)
  deriving (Typeable, Generic)

instance Binary Request

data Command = Get Key
             | Set Key Value
  deriving (Typeable, Generic)

instance Binary Command

data Response = ResGetResult (Maybe Value)
              | ResSetResult Bool
  deriving (Typeable, Generic)

instance Binary Response

createDB :: [NodeId] -> Process Database
createDB _ = spawnLocal $ do
  mvar <- liftIO $ newMVar M.empty
  forever $ do
    m <- expect
    handleMessage m mvar
    
handleMessage :: Request -> MVar (M.Map Key Value) -> Process ()

handleMessage (ReqOp (Get k) port) mvar = do
  map <- liftIO $ readMVar mvar
  let r = M.lookup k map
  sendChan port (ResGetResult r)

handleMessage (ReqOp (Set k v) port) mvar = do
  map <- liftIO $ takeMVar mvar
  let map' = M.insert k v map
  liftIO $ putMVar mvar map'
  sendChan port (ResSetResult $ (M.size map + 1) == M.size map')

set :: Database -> Key -> Value -> Process Bool
set db k v = do
  (sp, rp) <- newChan
  send db $ ReqOp (Set k v) sp
  ResSetResult r <- receiveChan rp
  return r

get :: Database -> Key -> Process (Maybe Value)
get db k = do
  (sp, rp) <- newChan
  send db $ ReqOp (Get k) sp
  ResGetResult r <- receiveChan rp
  return r

rcdata :: RemoteTable -> RemoteTable
rcdata = id
