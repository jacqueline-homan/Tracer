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
newtype RedirectTo = RedirectTo {unRedirectTo ∷ URI}
    deriving (Eq, Ord, Show, ToSURI)

-- | Prism that will only construct a 'RedirectTo' from a valid URI string.
redirectTo ∷ Prism' Text RedirectTo
redirectTo = prism'
    (view packed ∘ show ∘ unRedirectTo)
    (fmap RedirectTo ∘ parseAbsoluteURI ∘ view unpacked)

-- | The message we'll send in the body of the redirect.
newtype RedirectBody = RedirectBody {unRedirectBody ∷ Text}
    deriving (Eq, Ord, Show, ToMessage)
redirectBody ∷ Iso' Text RedirectBody
redirectBody = iso RedirectBody unRedirectBody

-- | Build a 'RedirectBody' from a list of 'Text' values.
redirectBody' ∷ ∀ (f ∷ * → *). Traversable f ⇒ Getter (f Text) RedirectBody
redirectBody' = to $ RedirectBody ∘ buildText ∘ hsep ∘ over traversed print

data App = App {
    _appRedirectTo ∷ RedirectTo,
    _appBody ∷ RedirectBody
    } deriving (Eq, Ord, Show)
appRedirectTo ∷ Lens' App RedirectTo
appRedirectTo = lens (_appRedirectTo) (\a r → a {_appRedirectTo = r})

appBody ∷ Lens' App RedirectBody
appBody = lens (_appBody) (\a b → a {_appBody = b})

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
-- Checks that we have at least two arguments, then builds 'App' in
-- 'Maybe' using 'Applicative'.
app ∷ Getter [Arg] (Maybe App)
app =
    let
        gs ∷ [Arg] → Maybe App
        gs (auri:msgs) = App
            <$> (auri ^. _Arg ^.pre redirectTo)
            ⊛ (Just ∘ view redirectBody' $ msgs & mapped %~ view _Arg)
        gs _ = Nothing
    in
        to gs

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
    a ← args

    maybe exitFail (server ∘ redirectApp) (a ^. app)
