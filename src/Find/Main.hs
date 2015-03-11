import System.Environment (getArgs)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.FilePath ((</>))
import Data.List (sort)
import Control.Concurrent.Async

main :: IO ()
main = do
  (command:filename:path:[]) <- getArgs
  case command of
    "find_seq" -> print =<< find_seq filename path
    "find_par" -> print =<< find_par filename path

find_seq :: String -> FilePath -> IO (Maybe FilePath)
find_seq filename dir = do
  fs <- getDirectoryContents dir
  let fs' = sort $ filter (`notElem` [".", ".."]) fs
  if any (== filename) fs'
    then return $ Just $ dir </> filename
    else loop fs'
  where
    loop [] = return Nothing
    loop (f:fs) = do
      let dir' = dir </> f
      isdir <- doesDirectoryExist dir'
      if isdir
        then do
          r <- find_seq filename dir'
          case r of
            Just _ -> return r
            Nothing -> loop fs
        else loop fs

find_par :: String -> FilePath -> IO (Maybe FilePath)
find_par filename dir = do
  fs <- getDirectoryContents dir
  let fs' = sort $ filter (`notElem` [".", ".."]) fs
  if any (== filename) fs'
    then return $ Just $ dir </> filename
    else do
      let ps = map (dir </>) fs'
      foldr (subfind filename) dowait ps []
  where
    dowait as = loop $ reverse as

    loop [] = return Nothing
    loop (a:as) = do
      r <- wait a
      case r of
        Nothing -> loop as
        Just p -> return $ Just p
  
subfind :: String -> FilePath
        -> ([Async (Maybe FilePath)] -> IO (Maybe FilePath))
        ->  [Async (Maybe FilePath)] -> IO (Maybe FilePath)
subfind filename dir inner asyncs = do
  isdir <- doesDirectoryExist dir
  if not isdir
    then inner asyncs
    else withAsync (find_par filename dir) $ \a -> inner (a:asyncs)

