{-# LANGUAGE NoImplicitPrelude #-}
module Run (run) where

import ConfigFile
import Import
import System.Directory
import Control.Error.Util

run :: RIO App ()
run = do
  app <- ask
  let options = appOptions app
  let optConfigFilePath = optionsConfigFile options 

  -- Output dir, either specified or pwd
  currentWorkingDir <- liftIO getCurrentDirectory
  let outputDir =  optionsOutputDir options ?: currentWorkingDir

  -- Config file, use the cli option, or search current working dir.
  maybeConfig <- liftIO $ getConfig optConfigFilePath 
  case maybeConfig of
    Nothing -> error $ "Could not find config file. Make sure you have '" <> defaultConfigFileName <> "' in the current dir, or any parent dir."
    Just config -> do

      cloneOrgConfigs outputDir (orgConfigs (configFile config))
      cloneUserConfigs outputDir (userConfigs (configFile config))

  
