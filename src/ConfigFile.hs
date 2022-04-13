{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module ConfigFile (getConfig) where

import Dhall
import Import
import RIO.Text hiding (all)
import System.Directory
import System.FilePath

getConfig :: IO (Maybe Config)
getConfig = do
    configFile <- findConfigFile
    case configFile of
        Nothing -> return Nothing
        Just file -> input auto $ pack file 

findConfigFile :: (MonadUnliftIO m) => m (Maybe FilePath)
findConfigFile = do
  currDir <- liftIO getCurrentDirectory
  path' <- liftIO $ canonicalizePath currDir
  
  let mkPaths p
        | all isPathSeparator p || p == "." = []
        --TODO: Change this filename
        | otherwise = (p </> "config.dhall") : mkPaths (takeDirectory p)
  let paths = mkPaths path'
  liftIO $ firstMaybeConfigFile paths

  -- I don't like this.
  where firstMaybeConfigFile :: [FilePath] -> IO (Maybe FilePath)
        firstMaybeConfigFile [] = return Nothing
        firstMaybeConfigFile [x] = do
          exists <- doesFileExist x
          if exists then return $ Just x else return Nothing
        firstMaybeConfigFile (x:xs) = do
          exists <- doesFileExist x
          if exists then return $ Just x
          else firstMaybeConfigFile xs