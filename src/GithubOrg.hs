{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module GithubOrg (cloneOrgConfigs) where

import Conduit
import Data.Conduit.ConcurrentMap
import qualified Data.Conduit.List as CL
import qualified Data.ByteString as B
import qualified GitHub as GH
import GithubShared
import RIO
import RIO.Text hiding (length, filter, map)
import RIO.Directory
import qualified RIO.Vector as V
import System.FilePath
import Types

filterRepos :: OrgConfig -> Vector GH.Repo -> Vector GH.Repo
filterRepos orgConfig = do
  V.filter (\r ->
    -- Forks
    GH.repoFork r /= Just True
    -- Owner login name /= orgName - like when a user is an owner of a repo in another org.
    && GH.untagName (GH.simpleOwnerLogin $ GH.repoOwner r) == pack (orgName orgConfig)
    -- Ignore list in config
    && notElem (unpack (GH.untagName $ GH.repoName r)) (ignoringOrgRepos orgConfig)
    )

getReposForOrgGithubCom :: (MonadReader env m,
                            MonadUnliftIO m,
                            HasLogFunc env) => OrgConfig -> m (Maybe (Vector GH.Repo))
getReposForOrgGithubCom orgConfig = do
  let gh = ghClient $ orgAPIToken orgConfig
  let ghOrgName = GH.mkOrganizationName $ pack $ orgName orgConfig
  possibleRepos <- liftIO $ gh GH.organizationReposR ghOrgName GH.RepoPublicityAll GH.FetchAll
  case possibleRepos of
    (Left err) -> do
      logInfo $ displayShow err
      return Nothing
    (Right repos) -> do
      return $ Just repos

getReposForOrgConfig :: (MonadReader env m,
                         MonadUnliftIO m,
                         HasLogFunc env) => OrgConfig -> m (Maybe (Vector GH.Repo), OrgConfig)
getReposForOrgConfig orgConfig = do
    repos <- getReposForOrgGithubCom orgConfig
    return (repos, orgConfig)

cloneSingleOrgConfig :: ( MonadReader env m,
                          MonadUnliftIO m,
                          HasLogFunc env) => FilePath -> Maybe (Vector GH.Repo) -> OrgConfig -> m ()
cloneSingleOrgConfig topDir maybeRepos orgConfig = do
  let topLevelName = orgName orgConfig
  let orgDir = topDir </> topLevelName

  -- Get relative to current working dir
  currDir <- liftIO getCurrentDirectory
  let relativeOrgDir = makeRelative currDir orgDir
  logInfo $ displayShow relativeOrgDir

  logSticky $ "Cloning the contents of: " <> displayShow topLevelName
  case maybeRepos of
    Nothing -> return ()
    Just repos'' -> do
      let repos = filterRepos orgConfig repos''
      let indexedRepos = getZipSource $ (,)
                            <$> ZipSource (yieldMany ([1..] :: [Int]))
                            <*> ZipSource (CL.sourceList (toList repos))

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

      let errors = filter (/= ExitSuccess) res
      unless (length errors == 0) (error "Failed cloning org")

      -- Restore dir
      setCurrentDirectory currDir
      logStickyDone ""
      return ()

-- for orgs in config, get repos, pick first one for now
-- if the dir doesn't exist, clone the repo, otherwise warn and skip
cloneOrgConfigs :: (MonadReader env m,
                 MonadUnliftIO m,
                 HasLogFunc env) => FilePath -> Vector OrgConfig -> m ()
cloneOrgConfigs topDir orgConfigs' = do
  -- First, we want to request all the repos upfront from all configs
  let indexedConfigs = getZipSource $ (,)
                          <$> ZipSource (yieldMany ([1..] :: [Int]))
                          <*> ZipSource (CL.sourceList (toList orgConfigs'))
  _ <- runConduitRes
              $ indexedConfigs
              -- Log sticky
              .| mapMC (\(idx, orgConfig) -> do
                  logSticky $ "Fetching Config Repos " <> displayShow idx <> "/" <> displayShow (length orgConfigs')
                  return orgConfig
                )
              -- Fetch list of repos
              .| concurrentMapM_ 8 10 getReposForOrgConfig
              -- Each thing coming through here should be (Maybe (Vector Repo), OrgConfig)
              -- So now we should just be able to hand this off to cloneSingleOrgConfig. Run one at a time, but internally concurrent.
              .| mapMC (\(maybeRepos, orgConfig) -> do
                  cloneSingleOrgConfig topDir maybeRepos orgConfig
                )
              .| CL.consume

  logSticky "All Done!!"
