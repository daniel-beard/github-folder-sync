{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import RIO.Process
import RIO.Text hiding (length, filter)
import GitHub (github')
import GitHub
import RIO.Directory
import qualified RIO.Vector as V
import System.FilePath

import RIO.Vector.Partial

getRepos :: (MonadReader env m, MonadUnliftIO m) => m (Maybe (Vector Repo))
getRepos = do
  possibleRepos <- liftIO $ github' GitHub.userReposR "daniel-beard" GitHub.RepoPublicityAll FetchAll
  case possibleRepos of
       (Left _)  -> return Nothing
       (Right repos) -> return $ Just repos
  -- where printRepos repos = mapM_ (logInfo . displayShow . repoCloneUrl) repos

getReposForUserOrOrg :: (MonadReader env m, MonadUnliftIO m, HasLogFunc env) => String -> m (Maybe (Vector Repo))
getReposForUserOrOrg orgOrUser = do
  -- This endpoint returns repos for either user or org, and has enough info that we can use it.
  possibleRepos <- liftIO $ github' GitHub.userReposR (mkOwnerName $ pack orgOrUser) GitHub.RepoPublicityAll FetchAll
  case possibleRepos of
    (Left err) -> do
      logInfo $ displayShow err
      return Nothing
    (Right repos) -> return $ Just repos

cloneSingleRepo :: (MonadReader env m,
                    MonadUnliftIO m,
                    HasProcessContext env,
                    HasLogFunc env) => FilePath -> Repo -> m Bool
cloneSingleRepo orgDir repo = do
  logInfo $ "Ensuring directory exists: " <> displayShow orgDir
  createDirectoryIfMissing True orgDir
  setCurrentDirectory orgDir
  -- Check if the repo folder exists, if not clone it
  let repoFolder = orgDir </> unpack (untagName $ repoName repo)
  let repoGitFolder = repoFolder </> ".git"
  repoGitFolderExists <- doesPathExist repoGitFolder
  let cloneSshURL = repoSshUrl repo
  if repoGitFolderExists then do
    logInfo $ "Skipping: looks like there's already a git repo at: " <> displayShow repoGitFolder
    return False
  else
    case cloneSshURL of
      Nothing -> return False
      Just sshURL -> do
        logInfo $ "Running: 'git clone " <> displayShow (unpack (getUrl sshURL)) <> "'"
        proc "git" ["clone", unpack (getUrl sshURL)] runProcess_ 
        return True

ensureSingleOrgConfigMatchesOnDiskContents :: (MonadReader env m,
                                               MonadUnliftIO m,
                                               HasProcessContext env,
                                               HasLogFunc env) => FilePath -> OrgConfig -> m ()
ensureSingleOrgConfigMatchesOnDiskContents topDir orgConfig = do
  let topLevelName = orgName orgConfig
  let orgDir = topDir </> topLevelName
  logInfo $ displayShow orgDir
  currDir <- getCurrentDirectory
  repos' <- getReposForUserOrOrg topLevelName

  case repos' of
    Nothing -> return ()
    Just repos'' -> do
      -- Filtering out following for now:
      --   - Forks
      --   - Anything where the SimpleOwnerLogin doesn't match the orgName for the given config
      --      - This happens for things like when a user is an owner of a repo in another org.
      let repos = V.filter (\r -> repoFork r /= Just True &&
                                untagName (simpleOwnerLogin $ repoOwner r) == pack topLevelName) repos''

      logInfo "Filtered Repo names: "
      mapM_ (logInfo . displayShow . repoUrl) repos

      --TODODB: Error handling and concurrency
      mapM_ (cloneSingleRepo orgDir) repos

      -- Restore dir
      setCurrentDirectory currDir
      return ()


-- TODODB: Need the enterprise version of above

-- for orgs in config, get repos, pick first one for now
-- if the dir doesn't exist, clone the repo, otherwise warn and skip


-- ensureConfigMatchesOnDiskFolders :: (MonadReader env m, MonadUnliftIO m, HasLogFunc env) => m ()
-- ensureConfigMatchesOnDiskFolders = do
--   mapM_ 



run :: RIO App ()
run = do
  env <- ask
  let c = config env
  logInfo $ displayShow c

  let firstConfig = head $ orgConfigs $ configFile c
  ensureSingleOrgConfigMatchesOnDiskContents (topLevelDir c) firstConfig
  -- orgOrUserRepos <- getReposForUserOrOrg "daniel-beard"
  -- logInfo $ "Got result: " <> displayShow orgOrUserRepos
  -- getRepos 
  -- proc "git" ["ls-files"] runProcess_
