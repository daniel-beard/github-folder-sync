{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import RIO.Process
import GitHub (github')
import GitHub

-- getReposRIO :: HasLogFunc env => RIO env ()
-- getReposRIO = getRepos

getRepos :: ( MonadReader env m
     , MonadUnliftIO m
     , HasLogFunc env) => RIO ()
getRepos = do
  possibleRepos <- github' GitHub.userReposR "daniel-beard" GitHub.RepoPublicityAll FetchAll
  logInfo possibleRepos
  -- case possibleRepos of
  --      (Left error)  -> logError "Error: "
  --      (Right repos) -> logInfo $ repos -- mapM_ (logInfo . formatRepo) repos

-- formatRepo repo =
--   (unpack $ GitHub.untagName $ GitHub.repoName repo) ++ "\t"
    -- (fromMaybe "" $ GitHub.repoDescription repo) ++ "\n"
    -- (GitHub.repoHtmlUrl repo) ++ "\n" ++
    -- (fromMaybe "" $ GitHub.repoCloneUrl repo) ++ "\t" ++
    -- "watchers: " ++ (show $ GitHub.repoWatchersCount repo) ++ "\t" 

run :: RIO App ()
run = do
  getRepos
  proc "git" ["ls-files"] runProcess_
  -- logInfo output
  logInfo "We're inside the application!"
