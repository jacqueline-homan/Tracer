module Main where

import Lawless
import Data.Time.Format
import Textual
import IO
import Data.String (IsString)

import Happstack.Server (nullConf,
    simpleHTTP,
    LogAccess,
    Conf(..),
    tempRedirect,
    ServerPartT,
    ToMessage)
import Happstack.Server.Monads (FilterMonad)
import Happstack.Server.Types (Response)

-- | Writes the salient information of a browser's request to the
-- terminal.
logRequest ∷ ∀ t. (FormatTime t) ⇒ LogAccess t
logRequest host _ time requestLine _ _ referer userAgent =
    hPutStrLn stderr $ print $ hsep [
    formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S.%q%z") time,
    print host,
    print referer,
    brackets $ print requestLine,
    brackets $ print referer,
    brackets $ print userAgent
    ]

serverConfig ∷ Conf
serverConfig = nullConf {logAccess = Just logRequest}

-- | Server for our redirect application.
server ∷ ToMessage a ⇒ ServerPartT IO a → IO ()
server = simpleHTTP serverConfig

-- | Redirection application that responds to all requests with a
-- redirection to our site.
redirectApp ∷ (IsString res, FilterMonad Response m) ⇒ m res
redirectApp = tempRedirect
    "https://www.patreon.com/teamunixman"
    "Server Response https://www.patreon.com/teamunixman"

main ∷ IO ()
main =
    server redirectApp
