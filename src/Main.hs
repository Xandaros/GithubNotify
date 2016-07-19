{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where
import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Data.Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Libnotify
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import System.Console.GetOpt
import System.Environment (getArgs)

import GithubNotify.Config
import GithubNotify.GNotification
import GithubNotify.GithubNotify

data Flag = ConfigFile FilePath
          | NoPrefetch
          deriving (Eq)

options :: [OptDescr Flag]
options = [ Option ['c'] ["config"] (ReqArg ConfigFile "asdf") "Read from specified config file"
          , Option ['n'] ["no-prefetch"] (NoArg NoPrefetch) "Do not display any notifications present at startup"
          ]

mkRequest :: BS8.ByteString -> GithubNotify Request
mkRequest lastModified = do
    tok <- authToken
    return (fromJust . parseRequest $ "https://api.github.com/notifications?access_token=" ++ tok) -- fromJust *should* be safe
           { requestHeaders = [ ("user-agent", "Xandaros")
                              , ("If-Modified-Since", lastModified)
                              ]
           , checkStatus = \status headers cookies ->
             let code = statusCode status
             in  if code >= 200 && code < 300 || code == 304
                 then Nothing
                 else Just . toException $ StatusCodeException status headers cookies
           }

parseArgs :: [String] -> IO ([Flag], [String])
parseArgs args = case getOpt Permute options args of
    (flags, nonOptions, []) -> return (flags, nonOptions)
    (_, _, errors)          -> ioError (userError (concat errors ++ usageInfo "" options))

getConfigFile :: [Flag] -> Maybe FilePath
getConfigFile [] = Nothing
getConfigFile (flag:flags) = case flag of
    ConfigFile fp -> Just fp
    _             -> getConfigFile flags

main :: IO ()
main = do
    (flags, nonOptions) <- getArgs >>= parseArgs

    configFile <- case getConfigFile flags of
        Just fp -> return fp
        Nothing -> ioError $ userError $ "No config file(-c) specified.\n" ++ usageInfo "" options

    config <- readConfigFile configFile
    case config of
        Nothing   -> error "Unable to read config file"
        Just conf -> withManager (lift . runGithubNotify conf . main' flags "")

main' :: [Flag] -> BS8.ByteString -> Manager -> GithubNotify ()
main' = main'' True

-- TODO: Cleanup
main'' :: Bool -> [Flag] -> BS8.ByteString -> Manager -> GithubNotify ()
main'' first flags lastNotifications manager = do
    req <- mkRequest lastNotifications
    res <- httpLbs req manager
    let headers = responseHeaders res
        body    = responseBody res
        code    = statusCode $ responseStatus res

    let pollInterval       = fromMaybe "60"              $ lookup "X-Poll-Interval" headers
        lastModified       = fromMaybe lastNotifications $ lookup "Last-Modified" headers
        remainingRateLimit = fromMaybe "unknown"         $ lookup "X-RateLimit-Remaining" headers
        gnotifications     = fromMaybe []                $ decode body

    unless (code == 304) $
        unless (first && NoPrefetch `elem` flags) $
            mapM_ showGNotification gnotifications

    liftIO $ do
        putStrLn $ "Fetched notifications: " ++ (show . length $ gnotifications)
        BS8.putStrLn $ "Remaining rate limit: " <> remainingRateLimit
        putStrLn ""
        threadDelay . (*1000000) . read . BS8.unpack $ pollInterval -- read is safe, as long as GitHub returns a number for X-Poll-Interval... FIXME

    main'' False flags lastModified manager

showGNotification :: GNotification -> GithubNotify ()
showGNotification gnotification = do
    customTimeout <- notificationTimeout
    let customTimeout' = fromMaybe Default $ Custom <$> customTimeout
    liftIO $ display_ ((summary . T.unpack $ gnotification ^. subject    . title)
                       <> (body . T.unpack $ gnotification ^. repository . full_name)
                       <> (timeout customTimeout')
                       )
