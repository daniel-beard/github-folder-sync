{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module ConfigFile (configFileName, getConfig, findConfigFile) where

import Dhall
import Import
import RIO.Text hiding (all)
import System.Directory
import System.FilePath

--TODO: Change this filename
configFileName :: FilePath 
configFileName = "config.dhall"

getConfig :: IO (Maybe Config)
getConfig = do
    configFile <- findConfigFile
    case configFile of
        Nothing -> return Nothing
        Just file -> do
          config <- input auto $ pack file 
          return $ Just config

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