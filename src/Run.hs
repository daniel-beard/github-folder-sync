{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Dhall
import Import
import RIO.Process
import GitHub (github')
import GitHub

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

run :: RIO App ()
run = do
  config <- liftIO $ input auto "./config.dhall"
  logInfo $ displayShow (config :: Config)
  getRepos 
  count <- getRepoCount
  logInfo $ displayShow count
  proc "git" ["ls-files"] runProcess_
  -- logInfo output
  logInfo "We're inside the application!"
