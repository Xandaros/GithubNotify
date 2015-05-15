{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Config where

import Control.Lens

data Config = Config
    { _configToken :: String
    } deriving (Show)
makeFields ''Config

defaultConfig = Config "ace755130ccb1e794dfb50234df9c1847f250530"
