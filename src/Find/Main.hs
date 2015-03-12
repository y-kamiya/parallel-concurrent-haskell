import System.Environment (getArgs)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.FilePath ((</>))
import Data.List (sort)
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async

import qualified FindParSem as Sem
import qualified FindParSemIORef as IORef

main :: IO ()
main = do
  (command:args) <- getArgs
  case command of
    "find_seq" -> print =<< find_seq_main args
    "find_par" -> print =<< find_par_main args
    "find_par_sem" -> print =<< find_par_sem args
    "find_par_sem_ioref" -> print =<< find_par_sem_ioref args

find_par_sem :: [String] -> IO (Maybe FilePath)
find_par_sem (filename:dir:n:[]) = do
  sem <- Sem.newNBSem $ read n
  Sem.find sem filename dir

find_par_sem_ioref :: [String] -> IO (Maybe FilePath)
find_par_sem_ioref (filename:dir:[]) = do
  n <- getNumCapabilities
  sem <- IORef.newNBSem (if n == 1 then 0 else n * 4)
  IORef.find sem filename dir

find_seq_main :: [String] -> IO (Maybe FilePath)
find_seq_main (filename:dir:[]) = find_seq filename dir

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



find_par_main :: [String] -> IO (Maybe FilePath)
find_par_main (filename:dir:[]) = find_par filename dir

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

