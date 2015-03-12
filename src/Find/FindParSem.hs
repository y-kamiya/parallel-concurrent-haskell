{-# LANGUAGE BangPatterns #-}

module FindParSem where

import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.FilePath ((</>))
import Data.List (sort)
import Control.Exception (finally)
import Control.Concurrent
import Control.Concurrent.Async

newtype NBSem = NBSem (MVar Int)

newNBSem :: Int -> IO NBSem
newNBSem i = do
  m <- newMVar i
  return $ NBSem m

tryAcquireNBSem :: NBSem -> IO Bool
tryAcquireNBSem (NBSem m) = do
  modifyMVar m $ \i ->
    if i == 0
       then return (i, False)
       else let !z = i-1 in return (z, True)

releaseNBSem :: NBSem -> IO ()
releaseNBSem (NBSem m) = do
  modifyMVar m $ \i ->
    let !z = i+1 in return (z,())

subfind :: NBSem -> String -> FilePath
        -> ([Async (Maybe FilePath)] -> IO (Maybe FilePath))
        ->  [Async (Maybe FilePath)] -> IO (Maybe FilePath)
subfind sem filename dir inner asyncs = do
  isdir <- doesDirectoryExist dir
  if not isdir
    then inner asyncs
    else do
      q <- tryAcquireNBSem sem
      if q
        then do
          let dofind = find sem filename dir `finally` releaseNBSem sem
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
