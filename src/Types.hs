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

-- Sub configs that get loaded from the config file
data OrgConfig = OrgConfig
  { githubAPIEndpoint :: Maybe String 
  , orgName :: String
  -- , folderNameOverride :: String
  } deriving (Generic, Show)
instance FromDhall OrgConfig

-- Top level ConfigFile contents
data ConfigFile = ConfigFile
  { orgConfigs :: Vector OrgConfig
  } deriving (Generic, Show)
instance FromDhall ConfigFile

-- We return this from the ConfigFile module, to attach the directory to the config
data Config = Config 
  { configFile  :: ConfigFile
  , topLevelDir :: FilePath
  } deriving (Show)

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  , config :: Config
  -- Add other app-specific configuration information here
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })
