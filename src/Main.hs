module Main where

import Lawless
import Time
import Textual
import IO


import Happstack.Server (nullConf, simpleHTTP, toResponse, ok, LogAccess, Conf(..))

logRequest ∷ ∀ t. FormatTime t ⇒ LogAccess t
logRequest host user time requestLine responseCode size referer userAgent =
    putStrLn $ hsep [
    print time,
    referer,
    brackets requestLine,
    brackets referer,
    brackets userAgent
    ]

serverConfig ∷ Conf
serverConfig = nullConf {logAccess = Just logRequest}

main ∷ IO ()
main = simpleHTTP serverConfig $ ok "Hello, World!"
