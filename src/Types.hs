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
-- We assume that any config containing a githubAPIEndpoint value is Enterprise (for now)
data OrgConfig = OrgConfig
  { githubAPIEndpoint :: Maybe String 
  , githubAPIToken :: Maybe String
  , orgName :: String
  , ignoringRepos :: Vector String
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

githubAPIEndpointL :: Lens' OrgConfig (Maybe String)
githubAPIEndpointL = lens githubAPIEndpoint (\o e -> o { githubAPIEndpoint = e })

githubAPITokenL :: Lens' OrgConfig (Maybe String)
githubAPITokenL = lens githubAPIToken (\o t -> o { githubAPIToken = t })

orgNameL :: Lens' OrgConfig String
orgNameL = lens orgName (\o name -> o { orgName = name })

ignoringReposL :: Lens' OrgConfig (Vector String)
ignoringReposL = lens ignoringRepos (\o repos -> o { ignoringRepos = repos })