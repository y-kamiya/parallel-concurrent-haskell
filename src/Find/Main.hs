import System.Directory

main :: IO ()
main = do
  (command:pattern:path) <- getArgs
  case command of
    "find_seq" -> find_seq pattern path

find_seq :: IO ()
find_seq = do

