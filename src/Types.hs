{-# LANGUAGE NoImplicitPrelude #-}
module Types where

import RIO
import RIO.Process

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  , optionsGithubOrg :: !String
  }

-- Used for both github.com and enterprise.
data GithubConfig = GithubConfig
  { authToken :: !String,
    apiEndpoint :: !String
  }

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  -- Add other app-specific configuration information here
  -- , githubConfig :: GithubConfig
  }

-- class HasAuthToken env where
--   authTokenG :: SimpleGetter env String 

-- instance HasAuthToken App where
--   authTokenG = to (authToken . githubConfig)
-- instance HasAuthToken GithubConfig where
--   authTokenG = to authToken

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })
