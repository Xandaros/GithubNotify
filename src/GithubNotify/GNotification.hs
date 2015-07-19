{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module GithubNotify.GNotification where
import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Text as T

data User = User
    { _userLogin :: T.Text
    , _userIdentifier :: Int
    , _userAvatar_url :: T.Text
    , _userGravatar_id :: T.Text
    , _userTyp :: T.Text
    , _userSite_admin :: Bool
    } deriving (Show)

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

instance FromJSON Subject where
    parseJSON (Object v) = Subject <$> v .: "title"
                                   <*> v .: "type"
    parseJSON _ = mzero

data Repository = Repository
    { _repositoryIdentifier :: Int
    , _repositoryOwner :: User
    , _repositoryName :: T.Text
    , _repositoryFull_name :: T.Text
    , _repositoryDescription :: T.Text
    , _repositoryPrivate :: Bool
    , _repositoryFork :: Bool
    } deriving (Show)

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
    { _gNotificationIdentifier :: T.Text
    , _gNotificationRepository :: Repository
    , _gNotificationSubject :: Subject
    , _gNotificationReason :: Maybe T.Text
    , _gNotificationUnread :: Bool
    , _gNotificationUpdated_at :: Maybe T.Text
    , _gNotificationLast_read_at :: Maybe T.Text
    } deriving (Show)

instance FromJSON GNotification where
    parseJSON (Object v) = GNotification <$> v .:  "id"
                                         <*> v .:  "repository"
                                         <*> v .:  "subject"
                                         <*> v .:? "reason"
                                         <*> v .:  "unread"
                                         <*> v .:? "updated_at"
                                         <*> v .:? "last_read_at"

makeFields ''User
makeFields ''Subject
makeFields ''Repository
makeFields ''GNotification

