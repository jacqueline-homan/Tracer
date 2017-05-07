{-|
Module:             Main
Description:        Module for counting web site hits before redirecting visitors.
Copyright:          © 2017 All rights reserved.
Maintainer:
        Jacqueline Homan <jacquelinehoman7@gmail.com>
        Evan Cofsky <evan@theunixman.com>
Stability:          experimental
Portability:        POSIX
-}

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
    ToMessage(..))
import Happstack.Server.Monads (FilterMonad)
import Happstack.Server.Types (Response)
import Happstack.Server.SURI (ToSURI)

-- * Application Configuration

-- ** URLs and content body the 'redirectApp' uses to create a
-- response.

-- | The target 'URI' we'll be redirecting to.
newtype RedirectTo = RedirectTo {unRedirectTo :: URI}
    deriving (Eq, Ord, Show, ToSURI)

-- | This is a 'Prism' from any 'Text' value to a 'Maybe
-- RedirectTo'. This means that it will only create a 'Just
-- RedirectTo' when the 'Text' is a valid 'URI', and 'Nothing'
-- otherwise. We use this in 'redirectAppConfig' to figure out if we
-- can create a 'Just RedirectAppConfig' from the parameters we have.
--
-- 'Prism's work in two directions. In one direction, the "reverse"
-- direction, they take an input, and if the input is valid, they
-- return a 'Just' of the type's value.
--
-- In the "forward" direction, they return the valid value represented
-- by the underlying type. This is always going to be valid, because
-- the "reverse" operation has checked it for us.
--
-- We also use this in 'appConfig', which builds a 'RedirectAppConfig'
-- from the 'Text' extracted from the command line arguments:
--
-- @
--     uri  ^.pre redirectTo
-- @
--
-- This runs the 'preview' operator, the _reverse_ operator, which
-- takes our 'Text' argument, and if it can successfully convert it to
-- a URI, will hand us a 'Just' 'RedirectTo'. If it can't, it hands us
-- a 'Nothing'. And this is exactly what we need for figuring out if a
-- URI the person gave us is actually something a browser can use.
redirectTo :: Prism' Text RedirectTo
redirectTo =
    let
        uri r = (show $ unRedirectTo r) ^. packed
        checkUri t = parseAbsoluteURI (t ^. unpacked)
        redTo t = RedirectTo <$> checkUri t
    in
        prism' uri redTo

-- | The message we'll send in the body of the redirect.
newtype RedirectBody = RedirectBody {unRedirectBody :: [Text]}
    deriving (Eq, Ord, Show)

-- | Convert between a 'RedirectBody', a list of 'Text' inputs, and
-- the same list of 'Text' values that'll become the content of the
-- body.
--
-- The body content has much less strict formatting requirements than
-- the URI, and really doesn't have any at all, so we're using an
-- 'Iso' instead of a 'Prism'.
--
-- 'Iso's are full bi-directional transformations between two
-- different types where there's no failure case. And since we're
-- really just wrapping the 'Text' list so we can let the compiler
-- check we're not passing around the wrong 'Text's, but otherwise,
-- really, we want the 'Text' list as is.
--
-- In fact, the way be defined 'RedirectBody' set us up for a really
-- simple 'Iso'' definition. We've already defined the backwards
-- ('unRedirectBody') and forwards ('RedirectBody') functions, and
-- that's all the 'Iso'' needs.
redirectBody :: Iso' RedirectBody [Text]
redirectBody =
    iso unRedirectBody RedirectBody

-- | Extracts the 'Text' from our 'RedirectBody' that can be used as a
-- response to the browser.
--
-- Now, since we want to use this as a message to the browser, we
-- need to use the 'ToMessage' typeclass, which lets Happstack know
-- how to send it back. And since we're essentially wrapping 'Text',
-- we can mostly use it's 'ToMessage' instance. But since we've got
-- the added list around it, we have to provide a way to convert our
-- list of 'Text's to just a single 'Text' before we can really take
-- advantage of the already-existing 'ToMessage' instance.
redirectBodyText :: Getter RedirectBody Text
redirectBodyText =
    let
        get b = buildText $ hsep $ over traversed print $ b ^. redirectBody
    in
        to get

-- | This tells HappStack how to convert our 'RedirectBody' into a
-- message for the browser.
instance ToMessage RedirectBody where
    -- | Here we cheat, and basically return 'toContentType' on an
    -- 'undefined' of type 'Text'. Since the 'toContentType' doesn't
    -- actually use the argument, and just the type of it, this is
    -- fine. It's also a fairly common thing to do, despite many
    -- people's reservations.
    toContentType = const $ toContentType (undefined :: Text)

    -- | And here we take our 'redirectBodyText' 'Getter' to turn our
    -- 'RedirectBody' into a single 'Text', then just use the
    -- 'toResponse' already defined for 'Text' to get our 'Message'.
    toMessage b = toMessage $ b ^. redirectBodyText

-- ** The Configuration of the 'redirectApp' Itself

-- | The 'redirectApp' uses to tell browsers where to look for the
-- information they requested.
data RedirectAppConfig = RedirectAppConfig {
    -- | This is the 'URI' that we'd like users to be redirected to
    -- after we record their hit.
    _appRedirectTo :: RedirectTo,

    -- | This is the body of the response we send the browser when we
    -- send it the new location.
    _appBody :: RedirectBody
    } deriving (Eq, Ord, Show)

-- *** 'Lens'es for the 'RedirectAppConfig'

-- | A 'Lens' that focuses on the 'RedirectTo' portion of our
-- 'RedirectAppConfig'.
appRedirectTo :: Lens' RedirectAppConfig RedirectTo
appRedirectTo =
    let
        getr = _appRedirectTo
        setr c t = c {_appRedirectTo = t}
    in
        lens getr setr

-- | Lens which focuses on the 'RedirectBody' of the
-- 'RedirectAppConfig'.
appRedirectBody :: Lens' RedirectAppConfig RedirectBody
appRedirectBody =
    let
        getb = _appBody
        setb c b = c {_appBody = b}
    in
        lens getb setb

-- | Builds a 'RedirectAppConfig' from the list of command line arguments if
-- possible.
--
-- Now, here, we use a 'Getter', which is one of the lesser types in
-- the 'Lens' family. But for building our configuration, where we
-- know we'll never want to see the original list of 'Arg's again,
-- it's perfect.
--
-- Now, since the 'RedirectTo' is defined by a 'Prism', we can't build
-- our 'RedirectAppConfig' as a normal function. We have to build it
-- using an 'Applicative', in this case a 'Maybe', since that's the
-- 'Applicative' of the 'preview' operation.
--
-- So this function, really, takes our command line arguments, a list
-- of 'Arg's, and then tries to build the 'RedirectAppConfig'. The way
-- the 'Maybe' 'Applicative' works is that if you chain them together
-- with the '(<*>)' operator, if at any point in the chain, you get a
-- 'Nothing', the result of the entire chain is 'Nothing'.
--
-- And so, when we try and run our 'RedirectAppConfig' over the
-- 'Maybe' 'Applicative', if 'preview' of the 'redirectTo' 'Prism' on
-- it's 'Arg' fails, we get a 'Nothing' back. And the caller knows
-- that the configuration isn't valid, so we shouldn't try and
-- redirect people's browsers to something that'll make them go
-- somewhere unsavory.
--
-- Now, one other thing we need to check is that we have enough
-- arguments. If the user only gave us a URI, for example, but no
-- message, we also fail. We require a body for the response to the
-- browser, and it's better if the user gives it to us than if we're
-- left to our own devices. Believe me, if you'd met us, you'd know.
redirectAppConfig :: Getter [Arg] (Maybe RedirectAppConfig)
redirectAppConfig =
    let
        txts as = over mapped (view _Arg) as

        cfg (uri:msgs) = RedirectAppConfig
            <$> (uri ^.pre redirectTo)
            <*> pure (msgs ^.re redirectBody)
        cfg _ = Nothing

        argsCfg as = cfg $ txts as
    in
        to argsCfg

-- * Our request logger

-- | Writes the salient information of a browser's request to the
-- terminal on the "error" channel, which is really just a channel
-- used for diagnostic messages separate from normal output.
logRequest :: forall t. (FormatTime t) => LogAccess t
logRequest host _ time requestLine _ _ referer userAgent =
    hPutStrLn stderr $ print $ hsep [
    formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S.%q%z") time,
    print host,
    print referer,
    brackets $ print requestLine,
    brackets $ print referer,
    brackets $ print userAgent
    ]

-- * The RedirectAppConfiglication itself

-- | A 'Conf' that has the 'ServerPartT' use our 'logRequest' function
-- for logging all browser requests.
serverConfig :: Conf
serverConfig = nullConf {logAccess = Just logRequest}

-- | A general HTTP server using our 'serverConfig', and thus our
-- 'logRequest' logger.
--
-- This also runs in 'MonadIO', which is a generalized 'IO' typeclass.
--
-- Any effect stack can be an instance of this typeclass, and most of
-- the <http://hackage.haskell.org/package/liblawless Lawless> package
-- is generalized for 'MonadIO'. When we use the 'putStrLn' from
-- 'Lawless', we get a lot of extra things that the regular version
-- doesn't have, including a lot of 'Text' and other type formatting
-- combiners, like 'hcat', 'hsep', 'fsep', and so on.
--
-- Most of the common types are also instances of the 'Printable'
-- typeclass provided by 'Textual', so they'll essentially work fine
-- with it. With the
-- <https://downloads.haskell.org/~ghc/8.0.2/docs/html/users_guide/glasgow_exts.html#overloaded-string-literals OverloadedStrings> language
-- extension, too, any literal text in double quotes "" can be
-- converted to any type that implements the
-- <http://hackage.haskell.org/package/base-4.9.1.0/docs/Data-String.html#t:IsString
-- IsString> typeclass.
server :: (MonadIO m, ToMessage a) => ServerPartT IO a -> m ()
server s =
    (putStrLn (hcat [
                               "You can view your application on http://localhost:",
                               print $ port serverConfig]))
    >> (liftIO $ simpleHTTP serverConfig s)

-- | Our request redirection application that responds to all requests
-- with a redirection to the site specified on the command line.
redirectApp :: FilterMonad Response m => RedirectAppConfig -> m RedirectBody
redirectApp a = tempRedirect (a ^. appRedirectTo) (a ^. appRedirectBody)

-- | Print an error message if we can't parse the URI or there's no
-- message.
exitFail :: IO ()
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

-- | This is the function that gets called first.
--
-- Here, we take the command line arguments from 'args', then try and
-- build a 'redirectAppConfig'. If that works, we run the 'server'
-- with a 'redirectApp' configured with this
-- 'redirectAppConfig'. Otherwise, we call our 'exitFail' function
-- which tries to print a helpful message to the user.
--
-- The 'maybe' function is a function that takes any 'Maybe', and if
-- it's value is a 'Just', it will run a function over the value of
-- the 'Just' and return the result of that function as a regular
-- value, i.e. not a 'Maybe' value. Otherwise, it calls the fallback
-- function, and returns that value instead.
--
-- Now, we're being a bit tricky here. The functions we're using both
-- return values of 'IO' '()', which is an effect type, and we're also
-- running them over the 'IO' type, because we're inside 'main'. So
-- what happens is that 'maybe' will return either our 'server' or
-- it'll return our 'exitFail', and regardless of which one is
-- returned, they'll be run by 'IO' for their effects. That's what
-- 'IO' does.
main :: IO ()
main =
    args
    >>= maybe exitFail (server . redirectApp) . view redirectAppConfig
