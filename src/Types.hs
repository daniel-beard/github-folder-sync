{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Types where

import RIO
import RIO.Process (HasProcessContext(..), ProcessContext)
import Data.Either.Validation (Validation(..))
import Dhall (FromDhall, ToDhall)
import qualified Dhall
import qualified Dhall.Core

-- | Command line arguments
data Options = Options
  { optionsVerbose    :: Bool
  , optionsConfigFile :: Maybe String
  , optionsOutputDir  :: Maybe String
  }

-- TODO: Think about this
-- data OrgExclusion = ExcludeOlderThan
-- TODO:
-- Consider having clone modes, like CloneAll, CloneOwnerOnly, CloneAllNoForks?
-- TODO: `ignoring*Repos` should be a Maybe.
-- TODO: Dhall is becoming a bit limiting to deal with defaults etc. Consider hand rolling a parser.

-- Sub configs that get loaded from the config file

data UserConfig = UserConfig
  { userName :: String
  , userAPIToken :: Maybe String
  , ignoringUserRepos :: Vector String
  } deriving (Generic, Show, FromDhall, ToDhall )

data OrgConfig = OrgConfig
  { orgName :: String
  , orgAPIToken :: Maybe String
  , ignoringOrgRepos :: Vector String
  } deriving (Generic, Show, FromDhall, ToDhall)

-- Top level ConfigFile contents
data ConfigFile = ConfigFile
  { orgConfigs :: Vector OrgConfig
  , userConfigs :: Vector UserConfig
  } deriving (Generic, Show, FromDhall, ToDhall)

-- We return this from the ConfigFile module, to attach the directory to the config
data Config = Config 
  { configFile  :: ConfigFile
  , configFilePath :: FilePath
  } deriving (Show)

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  }

-- Generate the canonical config file type from the types in this file
-- We inject this when parsing a user config file
dhallTypeFromConfigFile :: Text
dhallTypeFromConfigFile = do
   case Dhall.expected (Dhall.auto @ConfigFile) of
        Success result -> Dhall.Core.pretty result
        Failure _ -> "" -- should this error out?

dhallUserConfigType :: Text
dhallUserConfigType = do
  case Dhall.expected (Dhall.auto @UserConfig) of 
        Success result -> "let UserConfig : Type = " <> Dhall.Core.pretty result
        Failure _ -> "" -- should this error out?

dhallOrgConfigType :: Text
dhallOrgConfigType = do 
  case Dhall.expected (Dhall.auto @OrgConfig) of 
        Success result -> "let OrgConfig : Type = " <> Dhall.Core.pretty result
        Failure _ -> ""

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

userConfigsL :: Lens' ConfigFile (Vector UserConfig)
userConfigsL = lens userConfigs (\c n -> c { userConfigs = n })

userNameL :: Lens' UserConfig String
userNameL = lens userName (\u name -> u { userName = name })

userAPITokenL :: Lens' UserConfig (Maybe String)
userAPITokenL = lens userAPIToken (\u t -> u { userAPIToken = t })
