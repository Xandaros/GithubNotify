{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GithubNotify.GithubNotify ( GithubNotify()
                                 , runGithubNotify
                                 , authToken
                                 , notificationTimeout
                                 ) where
import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans

import GithubNotify.Config

newtype GithubNotify a = GithubNotify { unGithubNotify :: ReaderT Config IO a }
    deriving (Functor, Applicative, Monad, MonadIO)

runGithubNotify :: Config -> GithubNotify a -> IO a
runGithubNotify config gn = runReaderT (unGithubNotify gn) config

readConfig :: Getting a Config a -> GithubNotify a
readConfig l = GithubNotify $ asks (view l)

authToken :: GithubNotify String
authToken = readConfig token

notificationTimeout :: GithubNotify (Maybe Int)
notificationTimeout = readConfig customTimeout
