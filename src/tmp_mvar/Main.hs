import Control.Concurrent
import System.IO
import System.Environment

import qualified Phonebook as P

main :: IO ()
main = do
  [arg] <- getArgs
  case arg of
    "phone" -> phone

phone :: IO ()
phone = do
  s <- P.new
  sequence_ [ P.insert s ("name" ++ show n) (show n) | n <- [1..1000] ]
  P.lookup s "name1" >>= print
  P.lookup s "dummy" >>= print
