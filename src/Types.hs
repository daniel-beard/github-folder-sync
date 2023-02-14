{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Types where

import RIO
import RIO.Process ( HasProcessContext(..), ProcessContext )
import Dhall (FromDhall)

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  }

-- TODO: Think about this
-- data OrgExclusion = ExcludeOlderThan

-- Sub configs that get loaded from the config file
-- We assume that any config containing a githubAPIEndpoint value is Enterprise (for now)

data UserConfig = UserConfig
  { userName :: String
  , userAPIToken :: Maybe String
  , ignoringUserRepos :: Vector String
  } deriving (Generic, Show)
instance FromDhall UserConfig

data OrgConfig = OrgConfig
  { orgName :: String
  , orgAPIToken :: Maybe String
  , ignoringOrgRepos :: Vector String
  } deriving (Generic, Show)
instance FromDhall OrgConfig

-- Top level ConfigFile contents
data ConfigFile = ConfigFile
  { orgConfigs :: Vector OrgConfig
  -- , userConfigs :: Vector UserConfig
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

-- Lenses
------------------------------------------------------------------------------------

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })

configFileL :: Lens' Config ConfigFile
configFileL = lens configFile (\c cf -> c { configFile = cf })

orgConfigsL :: Lens' ConfigFile (Vector OrgConfig)
orgConfigsL = lens orgConfigs (\c n -> c { orgConfigs = n })

orgAPITokenL :: Lens' OrgConfig (Maybe String)
orgAPITokenL = lens orgAPIToken (\o t -> o { orgAPIToken = t })

orgNameL :: Lens' OrgConfig String
orgNameL = lens orgName (\o name -> o { orgName = name })

ignoringOrgReposL :: Lens' OrgConfig (Vector String)
ignoringOrgReposL = lens ignoringOrgRepos (\o repos -> o { ignoringOrgRepos = repos })