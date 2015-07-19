{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Config where

import Control.Lens
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Monoid

data Config = Config
    { _configToken         :: String
    , _configCustomTimeout :: Maybe Int
    } deriving (Show)

instance FromJSON Config where
    parseJSON (Object v) = Config <$> v .:  "token"
                                  <*> v .:? "timeout"
    parseJSON _ = mzero

makeFields ''Config

readConfigFile :: FilePath -> IO (Maybe Config)
readConfigFile path = decode <$> BL8.readFile path
