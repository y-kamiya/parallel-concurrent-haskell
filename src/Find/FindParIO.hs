module FindParIO where

import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.FilePath ((</>))
import Data.List (sort)
-- import Control.Monad.Par
import Control.Monad.Par.IO
import Control.Monad.Par.Class
import Control.Monad.IO.Class (liftIO)

subfind :: String -> FilePath
        -> ([IVar (Maybe FilePath)] -> ParIO (Maybe FilePath))
        ->  [IVar (Maybe FilePath)] -> ParIO (Maybe FilePath)
subfind filename dir inner ivars = do
  isdir <- liftIO $ doesDirectoryExist dir
  if not isdir
    then inner ivars
    else do
      v <- new
      fork $ find filename dir >>= put v
      inner $ v : ivars

find :: String -> FilePath -> ParIO (Maybe FilePath)
find filename dir = do
  fs <- liftIO $ getDirectoryContents dir
  let fs' = sort $ filter (`notElem` [".", ".."]) fs
  if any (== filename) fs'
    then return $ Just $ dir </> filename
    else do
      let ps = map (dir </>) fs'
      foldr (subfind filename) dowait ps []
    where
      dowait vs = loop $ reverse vs

      loop [] = return Nothing
      loop (v:vs) = do
        r <- get v
        case r of
          Nothing -> loop vs
          Just p -> return $ Just p

  -- fs <- getDirectoryContents dir
  -- let fs' = sort $ filter (`notElem` [".", ".."]) fs
  -- if any (== filename) fs'
  --   then return $ Just $ dir </> filename
  --   else do
  --     let ps = map (dir </>) fs'
  --     foldr (subfind filename) dowait ps []
  -- where
  --   dowait as = loop $ reverse as

  --   loop [] = return Nothing
  --   loop (a:as) = do
  --     r <- wait a
  --     case r of
  --       Nothing -> loop as
  --       Just p -> return $ Just p
