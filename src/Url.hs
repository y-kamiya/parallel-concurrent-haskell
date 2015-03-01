module Url where

import Network.HTTP

sites :: [String]
sites = [ "http://hackage.haskell.org/package/stm-2.4.4/docs/Control-Concurrent-STM-TVar.html"
        , "http://hackage.haskell.org/package/stm-2.4.4/docs/Control-Concurrent-STM-TMVar.html"
        , "http://hackage.haskell.org/package/stm-2.4.4/docs/Control-Concurrent-STM.html"
        ]

getURL :: String -> IO String
getURL url = simpleHTTP (getRequest url) >>= getResponseBody

