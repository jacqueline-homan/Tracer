{-# Language TemplateHaskell #-}

module Main where

import Lawless
import Data.Time.Format
import Textual
import IO
import Network.URI
import Text
import Environment
import System.Exit

import Happstack.Server (nullConf,
    simpleHTTP,
    LogAccess,
    Conf(..),
    tempRedirect,
    ServerPartT,
    ToMessage)
import Happstack.Server.Monads (FilterMonad)
import Happstack.Server.Types (Response)
import Happstack.Server.SURI (ToSURI)

-- * Type definitions

-- | The target we'll be redirecting to.
newtype RedirectTo = RedirectTo URI deriving (Eq, Ord, Show, ToSURI)
makePrisms ''RedirectTo

-- | The message we'll send in the body of the redirect.
newtype RedirectBody = RedirectBody Text deriving (Eq, Ord, Show, ToMessage)
makePrisms ''RedirectBody

data App = App {
    _appRedirectTo ∷ RedirectTo,
    _appBody ∷ RedirectBody
    } deriving (Eq, Ord, Show)
makeLenses ''App

-- * Our request logger

-- | Writes the salient information of a browser's request to the
-- terminal on the "error" channel, which is really just a channel
-- used for diagnostic messages separate from normal output.
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

-- * The Application itself

-- | A 'Conf' that has the 'ServerPartT' use our 'logRequest' function
-- for logging all browser requests.
serverConfig ∷ Conf
serverConfig = nullConf {logAccess = Just logRequest}

-- | A general HTTP server using our 'serverConfig', and thus our
-- 'logRequest' logger.
server ∷ ToMessage a ⇒ ServerPartT IO a → IO ()
server = simpleHTTP serverConfig

-- | Our request redirection application that responds to all requests
-- with a redirection to the site specified on the command line.
redirectApp ∷ FilterMonad Response m ⇒ App → m RedirectBody
redirectApp a = tempRedirect (a ^. appRedirectTo) (a ^. appBody)

-- | Builds an 'App' from the list of command line arguments if
-- possible.
--
-- First we check that we have at least two arguments, one for the
-- redirect target, the other for the redirect message. Then, we
-- create the 'RedirectTo' by parsing the URI, which also may fail,
-- and converting the body message straight to a 'RedirectBody' which
-- won't fail.
--
-- By taking the arguments as a parameter, we don't need access to
-- 'IO' from this function, and we can leave all of that in 'main'.
app ∷ [Arg] → Maybe App
app as =
    let
        -- Check the length of the arguments. If there aren't at least
        -- two, we return Nothing. Otherwise, we return the list of
        -- arguments.
        alen ∷ Maybe [Arg]
        alen = if (lengthOf traversed as < 2) then Nothing else Just as

        -- Parse an 'Arg' and if successful, return a 'RedirectTo'.
        rt ∷ Arg → Maybe RedirectTo
        rt auri = (RedirectTo <$> parseAbsoluteURI (auri ^. unpacked))

        -- Join a series of 'Arg's into a single 'RedirectBody'
        -- separated by spaces.
        msg ∷ [Arg] → Maybe RedirectBody
        msg = Just ∘ RedirectBody ∘ buildText ∘ hsep ∘ over traversed print
    in
        alen ≫= \(auri:msgs) → App <$> rt auri ⊛ msg msgs

-- | Print an error message if we can't parse the URI or there's no
-- message.
exitFail ∷ IO ()
exitFail = do
    pn ← progName
    hPutStrLn stderr "Couldn't parse arguments. Usage:"
    hPutStrLn stderr ""
    hPutStrLn stderr $ hsep [print pn, "<target-uri>", "<message>", "[message…]"]
    hPutStrLn stderr ""
    hPutStrLn stderr
        "This will redirect all requests to <target-uri>, and combine the rest"
    hPutStrLn stderr " of the arguments into a message for the response body."
    hPutStrLn stderr ""

    exitWith $ ExitFailure 1

main ∷ IO ()
main = do
    a ← app <$> args
    case a of
        Just p → server $ redirectApp p
        Nothing → exitFail
