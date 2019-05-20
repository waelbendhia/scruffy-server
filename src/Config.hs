{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Config ( DBConfig(..), Config(..), readConf ) where

import           Data.Aeson
import           Data.Yaml

import           Database.Beam.Postgres

import           GHC.Generics

import           System.Console.CmdArgs as C

newtype DBConfig = DBConfig { unConfig :: ConnectInfo }
    deriving ( Show, Generic )

data Config = Config { database :: DBConfig, port :: Int }
    deriving ( Show, Generic )

instance FromJSON DBConfig where
    parseJSON v = DBConfig <$> genericParseJSON defaultOptions v

instance FromJSON Config

newtype CmdArg = CmdArg { config :: String }
    deriving ( Show, Data, Typeable )

getConfigPath :: IO String
getConfigPath = config <$>
    cmdArgs (CmdArg { config = "./conf.yml" &= help "Config file path" }
             &= summary "Scruffy Server")

readConf :: IO Config
readConf = getConfigPath >>= decodeFileThrow

