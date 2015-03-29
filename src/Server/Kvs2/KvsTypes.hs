{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}

module Kvs2.KvsTypes where

import GHC.Generics
import Data.Typeable
import Data.Binary
import Control.Distributed.Process hiding (handleMessage)

type Key = String
type Value = String

type Database = ProcessId

data Request = ReqOp Command (SendPort Response)
  deriving (Show, Typeable, Generic)

instance Binary Request

data Command = Get Key
             | Set Key Value
  deriving (Show, Typeable, Generic)

instance Binary Command

data Response = ResGetResult (Maybe Value)
              | ResSetResult Bool
  deriving (Typeable, Generic)

instance Binary Response

