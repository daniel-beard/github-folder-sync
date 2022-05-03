{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module ConfigFile (configFileName, getConfig) where

import Dhall
import Import
import RIO.Text hiding (all)
import System.Directory
import System.FilePath

--TODO: Change this filename
configFileName :: FilePath 
configFileName = "config.dhall"

-- Overridden for now for easier testing
getConfig :: IO (Maybe Config)
getConfig = do
  currDir <- getCurrentDirectory 
  rawConfig <- detailed $ input auto $ pack $ currDir </> ".." </> "folder-sync" </> configFileName
  return $ Just Config { configFile = rawConfig, topLevelDir = currDir </> ".." </> "folder-sync" }

-- getConfig :: IO (Maybe Config)
-- getConfig = do
--     maybeConfigFile <- findConfigFile
--     case maybeConfigFile of
--         Nothing -> return Nothing
--         Just file -> do
--           let configDirectory = takeDirectory  file
--           rawConfig <- detailed $ input auto $ pack file 
--           return $ Just Config { configFile = rawConfig, topLevelDir = configDirectory } 

findConfigFile :: (MonadUnliftIO m) => m (Maybe FilePath)
findConfigFile = do
  currDir <- liftIO getCurrentDirectory
  path' <- liftIO $ canonicalizePath currDir
  let mkPaths p
        | all isPathSeparator p || p == "." = []
        | otherwise = (p </> configFileName) : mkPaths (takeDirectory p)
  liftIO $ firstMaybeConfigFile $ mkPaths path' 
  where firstMaybeConfigFile :: [FilePath] -> IO (Maybe FilePath)
        firstMaybeConfigFile [] = return Nothing
        firstMaybeConfigFile [x] = do
          exists <- doesFileExist x
          if exists then return $ Just x else return Nothing
        firstMaybeConfigFile (x:xs) = do
          exists <- doesFileExist x
          if exists then return $ Just x
          else firstMaybeConfigFile xs