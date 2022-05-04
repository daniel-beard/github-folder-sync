{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import Conduit
import Data.Conduit.ConcurrentMap
import qualified Data.Conduit.Process.Typed as PT
import qualified Data.Conduit.List as CL
import qualified Data.ByteString as B
import GitHub
import RIO.Process
import RIO.Text hiding (length, filter)
import RIO.Directory
import qualified RIO.Vector as V
import System.FilePath


import RIO.Vector.Partial

getReposForUserOrOrgGithubCom :: (MonadReader env m,
                                  MonadUnliftIO m,
                                  HasLogFunc env) => String -> m (Maybe (Vector Repo))
getReposForUserOrOrgGithubCom orgOrUser = do
  -- This endpoint returns repos for either user or org, and has enough info that we can use it.
  possibleRepos <- liftIO $ github' GitHub.userReposR (mkOwnerName $ pack orgOrUser) GitHub.RepoPublicityAll FetchAll
  case possibleRepos of
    (Left err) -> do
      logInfo $ displayShow err
      return Nothing
    (Right repos) -> return $ Just repos

getReposForUserOrOrgGithubEnterprise :: (MonadReader env m,
                                         MonadUnliftIO m,
                                         HasLogFunc env) => String -> OrgConfig -> m (Maybe (Vector Repo))
getReposForUserOrOrgGithubEnterprise orgOrUser orgConfig = do
  -- This endpoint returns repos for either user or org, and has enough info that we can use it.
  -- possibleRepos <- liftIO $ github' GitHub.userReposR (mkOwnerName $ pack orgOrUser) GitHub.RepoPublicityAll FetchAll
  possibleRepos <- liftIO $ github
                  (GitHub.EnterpriseOAuth
                    (fromString $ fromMaybe "" $ githubAPIEndpoint orgConfig)
                    (fromString $ fromMaybe "" $ githubAPIToken orgConfig)
                  ) GitHub.userReposR (mkOwnerName $ pack orgOrUser) GitHub.RepoPublicityAll FetchAll
  case possibleRepos of
    (Left err) -> do
      logInfo $ displayShow err
      return Nothing
    (Right repos) -> return $ Just repos

getReposForUserOrOrg :: (MonadReader env m,
                         MonadUnliftIO m,
                         HasLogFunc env) => String -> OrgConfig -> m (Maybe (Vector Repo))
getReposForUserOrOrg orgOrUser orgConfig = do
  case (githubAPIEndpoint orgConfig) of
    Nothing -> getReposForUserOrOrgGithubCom orgOrUser
    Just apiEndpoint -> getReposForUserOrOrgGithubEnterprise orgOrUser orgConfig 

cloneSingleRepo :: (MonadReader env m,
                    MonadUnliftIO m,
                    HasLogFunc env) => FilePath -> Repo -> m ([ByteString], ExitCode)
cloneSingleRepo orgDir repo = do
  createDirectoryIfMissing True orgDir
  setCurrentDirectory orgDir
  -- Check if the repo folder exists, if not clone it
  let repoFolder = orgDir </> unpack (untagName $ repoName repo)
  let repoGitFolder = repoFolder </> ".git"
  repoGitFolderExists <- doesPathExist repoGitFolder
  let cloneSshURL = repoSshUrl repo
  if repoGitFolderExists then do
    logInfo $ "Skipping: looks like there's already a git repo at: " <> displayShow repoGitFolder
    return ([], ExitSuccess)
  else
    case cloneSshURL of
      Nothing -> return ([], ExitSuccess)
      Just sshURL -> do
        logInfo $ "Running: 'git clone " <> displayShow (unpack (getUrl sshURL)) <> "'"
        let pc = setStderr PT.createSource $ PT.proc "git" ["clone", unpack (getUrl sshURL)]
        withProcessWait pc $ \p ->
          runConduit (getStderr p .| CL.consume) `concurrently`
          runConduit (waitExitCode p)



cloneSingleOrgConfig :: ( MonadReader env m,
                          MonadUnliftIO m,
                          HasLogFunc env) => FilePath -> OrgConfig -> m ()
cloneSingleOrgConfig topDir orgConfig = do
  let topLevelName = orgName orgConfig
  let orgDir = topDir </> topLevelName
  logInfo $ displayShow orgDir
  currDir <- getCurrentDirectory

  --
  repos' <- getReposForUserOrOrg topLevelName orgConfig

  logSticky $ "Cloning the contents of: " <> displayShow topLevelName
  case repos' of
    Nothing -> return ()
    Just repos'' -> do
      -- Filter repos
      let repos = V.filter (\r -> 
                                  -- Forks
                                  repoFork r /= Just True 
                                  -- Owner login name /= orgName - like when a user is an owner of a repo in another org.
                                  && untagName (simpleOwnerLogin $ repoOwner r) == pack topLevelName 
                                  -- Ignore list in config
                                  && notElem (unpack (untagName $ repoName r)) (ignoringRepos orgConfig)
                                ) repos'' 

      let indexedRepos = getZipSource $ (,)
                            <$> ZipSource (yieldMany ([1..] :: [Int]))
                            <*> ZipSource (CL.sourceList (toList repos))

      --TODODB: Test the error handling
      res <- runConduitRes
                 $ indexedRepos
                .| mapMC (\(idx, result) -> do
                    logSticky $ "Cloning " <> displayShow idx <> "/" <> displayShow (length repos)
                    return result
                  )
                .| concurrentMapM_ 8 10 (cloneSingleRepo orgDir)
                .| mapMC (\(stderr', exitcode) -> do
                    case exitcode of
                      ExitSuccess -> return exitcode
                      ExitFailure n -> do
                        logError $ "FAILED w/code: " <> displayShow n
                        logError $ displayShow $ decodeUtf8With lenientDecode $ B.intercalate "\n" stderr'
                        return exitcode
                  )
                .| CL.consume

      --TODODB: Probably want to panic here if we have errors above?
      let errors = filter (/= ExitSuccess) res

      -- Restore dir
      setCurrentDirectory currDir
      logStickyDone ""
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
  -- logInfo $ displayShow c

  let firstConfig = head $ orgConfigs $ configFile c
  cloneSingleOrgConfig (topLevelDir c) firstConfig
