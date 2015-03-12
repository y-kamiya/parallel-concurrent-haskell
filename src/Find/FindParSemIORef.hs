{-# LANGUAGE BangPatterns #-}

module FindParSemIORef where

import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.FilePath ((</>))
import Data.List (sort)
import Data.IORef
import Control.Exception (finally)
import Control.Concurrent
import Control.Concurrent.Async

newtype NBSem = NBSem (IORef Int)

newNBSem :: Int -> IO NBSem
newNBSem i = do
  m <- newIORef i
  return $ NBSem m

tryWaitNBSem :: NBSem -> IO Bool
tryWaitNBSem (NBSem m) = do
  atomicModifyIORef m $ \i ->
    if i == 0
       then (i, False)
       else let !z = i-1 in (z, True)

signalNBSem :: NBSem -> IO ()
signalNBSem (NBSem m) = do
  atomicModifyIORef m $ \i ->
    let !z = i+1 in (z,())

subfind :: NBSem -> String -> FilePath
        -> ([Async (Maybe FilePath)] -> IO (Maybe FilePath))
        ->  [Async (Maybe FilePath)] -> IO (Maybe FilePath)
subfind sem filename dir inner asyncs = do
  isdir <- doesDirectoryExist dir
  if not isdir
    then inner asyncs
    else do
      q <- tryWaitNBSem sem
      if q
        then do
          let dofind = find sem filename dir `finally` signalNBSem sem
          withAsync dofind $ \a -> inner (a:asyncs)
        else do
          r <- find sem filename dir
          case r of
            Nothing -> inner asyncs
            Just _ -> return r

find :: NBSem -> String -> FilePath -> IO (Maybe FilePath)
find sem filename dir = do
  fs <- getDirectoryContents dir
  let fs' = sort $ filter (`notElem` [".", ".."]) fs
  if any (== filename) fs'
    then return $ Just $ dir </> filename
    else do
      let ps = map (dir </>) fs'
      foldr (subfind sem filename) dowait ps []
  where
    dowait as = loop $ reverse as

    loop [] = return Nothing
    loop (a:as) = do
      r <- wait a
      case r of
        Nothing -> loop as
        Just p -> return $ Just p
