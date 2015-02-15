module Phonebook where

import Control.Concurrent
import qualified Data.Map as M

type Name = String
type PhoneNumber = String
type PhoneBook = M.Map Name PhoneNumber

newtype PhoneBookState = PhoneBookState (MVar PhoneBook)

new :: IO PhoneBookState
new = do
  m <- newMVar M.empty
  return $ PhoneBookState m

insert :: PhoneBookState -> Name -> PhoneNumber -> IO ()
insert (PhoneBookState m) name number = do
  book <- takeMVar m
  putMVar m $ M.insert name number book

lookup :: PhoneBookState -> Name -> IO (Maybe PhoneNumber)
lookup (PhoneBookState m) name = do
  book <- takeMVar m
  putMVar m book
  return $ M.lookup name book


