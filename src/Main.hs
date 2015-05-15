{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- ace755130ccb1e794dfb50234df9c1847f250530
module Main where
import Control.Applicative
import Control.Concurrent (threadDelay)
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
import Libnotify
import Network.HTTP.Conduit

import Config

data User = User
    { _userLogin :: T.Text
    , _userIdentifier :: Int
    , _userAvatar_url :: T.Text
    , _userGravatar_id :: T.Text
    , _userTyp :: T.Text
    , _userSite_admin :: Bool
    } deriving (Show)
makeFields ''User

instance FromJSON User where
    parseJSON (Object v) = User <$> v .: "login"
                                <*> v .: "id"
                                <*> v .: "avatar_url"
                                <*> v .: "gravatar_id"
                                <*> v .: "type"
                                <*> v .: "site_admin"
    parseJSON _ = mzero

data Subject = Subject
    { _subjectTitle :: T.Text
    , _subjectTyp :: T.Text
    } deriving (Show)
makeFields ''Subject

instance FromJSON Subject where
    parseJSON (Object v) = Subject <$> v .: "title"
                                   <*> v .: "type"
    parseJSON _ = mzero

data Repository = Repository
    { _repositoryIdentifier :: T.Text
    , _repositoryOwner :: User
    , _repositoryName :: T.Text
    , _repositoryFull_name :: T.Text
    , _repositoryDescription :: T.Text
    , _repositoryPrivate :: Bool
    , _repositoryFork :: Bool
    } deriving (Show)
makeFields ''Repository

instance FromJSON Repository where
    parseJSON (Object v) = Repository <$> v .: "id"
                                      <*> v .: "owner"
                                      <*> v .: "name"
                                      <*> v .: "full_name"
                                      <*> v .: "description"
                                      <*> v .: "private"
                                      <*> v .: "fork"
    parseJSON _ = mzero

data GNotification = GNotification
    { _notificationIdentifier :: T.Text
    , _notificationRepository :: Repository
    , _notificationSubject :: Subject
    , _notificationReason :: Maybe T.Text
    , _notificationUnread :: Bool
    , _notificationUpdated_at :: Maybe T.Text
    , _notificationLast_read_at :: Maybe T.Text
    } deriving (Show)
makeFields ''GNotification

instance FromJSON GNotification where
    parseJSON (Object v) = GNotification <$> v .:  "id"
                                         <*> v .:  "repository"
                                         <*> v .:  "subject"
                                         <*> v .:? "reason"
                                         <*> v .:  "unread"
                                         <*> v .:? "updated_at"
                                         <*> v .:? "last_read_at"

newtype GithubNotify a = GithubNotify { unGithubNotify :: ReaderT Config IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config)

runGithubNotify :: Config -> GithubNotify a -> IO a
runGithubNotify config gn = runReaderT (unGithubNotify gn) config

mkRequest :: BS8.ByteString -> GithubNotify Request
mkRequest lastModified = do
    config <- ask
    let tok = config ^. token
    return (fromJust . parseUrl $ "https://api.github.com/notifications?access_token=" ++ tok) -- fromJust *should* be safe
           { requestHeaders = [ ("user-agent", "Xandaros")
                              , ("If-Modified-Since", lastModified)
                              ]
           }

main :: IO ()
main = withManager (lift . runGithubNotify defaultConfig . main' "")

-- TODO: Cleanup
main' :: BS8.ByteString -> Manager -> GithubNotify ()
main' lastNotifications manager = do
    req <- mkRequest lastNotifications
    res <- httpLbs req manager
    let headers = responseHeaders res
    let body = responseBody res
    let pollInterval = fromJust $ lookup "X-Poll-Interval" headers <|> Just "60" -- fromJust is safe

    let lastModified = fromJust $ lookup "Last-Modified" headers <|> Just lastNotifications -- fromJust is safe

    liftIO $ print headers
    liftIO $ putStrLn ""
    liftIO $ BL8.putStrLn body
    liftIO $ BS8.putStrLn $ "pollInterval: " <> pollInterval
    liftIO $ BS8.putStrLn $ "lastModified: " <> lastModified
    liftIO $ putStrLn ""
    liftIO $ print (decode body :: Maybe [GNotification])
    liftIO $ threadDelay (read (BS8.unpack pollInterval)*1000000)

    main' lastModified manager
