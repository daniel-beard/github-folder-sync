{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
module Types where

import RIO
import RIO.Process
import Dhall (FromDhall)

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  }

-- TODO: Think about this
-- data OrgExclusion = ExcludeOlderThan

data OrgConfig = OrgConfig
  { githubAPIEndpoint :: Maybe String 
  , orgName :: String
  -- , folderNameOverride :: String
  } deriving (Generic, Show)

instance FromDhall OrgConfig

data Config = Config
  { orgConfigs :: Vector OrgConfig
  } deriving (Generic, Show)

instance FromDhall Config

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  -- Add other app-specific configuration information here
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })
