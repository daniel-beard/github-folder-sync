{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Dhall
import Import
import RIO.Process
import GitHub (github')
import GitHub
import System.Directory
import System.FilePath

getRepos :: (MonadReader env m, MonadUnliftIO m, HasLogFunc env) => m ()
getRepos = do
  possibleRepos <- liftIO $ github' GitHub.userReposR "daniel-beard" GitHub.RepoPublicityAll FetchAll
  case possibleRepos of
       (Left error)  -> logError "Error: "
       (Right repos) -> printRepos repos
  where printRepos repos = mapM_ (logInfo . displayShow . repoCloneUrl) repos

getRepoCount :: (MonadReader env m, MonadUnliftIO m) => m Int
getRepoCount = do
  possibleRepos <- liftIO $ github' GitHub.userReposR "daniel-beard" GitHub.RepoPublicityAll FetchAll
  case possibleRepos of
       (Left error)  -> pure 0
       (Right repos) -> pure $ length repos

-- findConfigFile :: FilePath -> IO (Maybe FilePath)
findConfigFile :: (MonadReader env m, MonadUnliftIO m, HasLogFunc env) => m ()
findConfigFile = do
  currDir <- liftIO $ getCurrentDirectory
  path' <- liftIO $ canonicalizePath currDir
  
  let mkPaths p
        | all isPathSeparator p || p == "." 
                    = []
        --TODO: Change this filename
        | otherwise = (p </> "config.dhall") 
                    : mkPaths (takeDirectory p)

  --TODO: Return Maybe FilePath with just the first match
  let paths = mkPaths path'
  logInfo $ displayShow paths


run :: RIO App ()
run = do
  configFile <- findConfigFile
  config <- liftIO $ input auto "./config.dhall"
  logInfo $ displayShow (config :: Config)
  getRepos 
  count <- getRepoCount
  logInfo $ displayShow count
  proc "git" ["ls-files"] runProcess_
  logInfo "We're inside the application!"
