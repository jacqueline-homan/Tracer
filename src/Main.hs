module Main where

import Lawless
import Data.Time.Format
import Textual
import IO

import Happstack.Server (nullConf, simpleHTTP, toResponse, ok)

printTime =
    print ∘ formatTime defaultLocale (iso8601DateFormat (Just "%H:%M:%S.%q%z"))

logRequest ∷ LogAccess
logRequest host user time requestLine responseCode size referer userAgent =
    putStrLn $ hsep [
    printTime time,
    referer,
    brackets requestLine,
    brackets referer,
    brackets userAgent
    ]

serverConfig ∷ Conf
serverConfig = nullConf {logAccess = Just logRequest}

main :: IO ()
main = simpleHTTP serverConfig $ ok "Hello, World!"
