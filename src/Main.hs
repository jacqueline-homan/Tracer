module Main where

import Lawless
import Data.Time.Format
import Textual
import IO

import Happstack.Server (nullConf, simpleHTTP, LogAccess, Conf(..), tempRedirect)

logRequest ∷ ∀ t. (FormatTime t) ⇒ LogAccess t
logRequest host _ time requestLine _ _ referer userAgent =
    putStrLn $ hsep [
    formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S.%q%z") time,
    host,
    referer,
    brackets requestLine,
    brackets referer,
    brackets userAgent
    ]

serverConfig ∷ Conf
serverConfig = nullConf {logAccess = Just logRequest}

main ∷ IO ()
main = simpleHTTP serverConfig $ tempRedirect "https://www.patreon.com/teamunixman" "Server Response https://www.patreon.com/teamunixman"


