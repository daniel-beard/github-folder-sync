{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module GithubUser (cloneUserConfigs) where

import Conduit
import Data.Conduit.ConcurrentMap
import qualified Data.Conduit.List as CL
import qualified Data.ByteString as B
import qualified GitHub as GH
import GithubShared
import RIO
import RIO.Text hiding (length, filter, map)
import qualified RIO.Vector as V
import System.FilePath
import Types

getReposForUserGithubCom :: (MonadReader env m,
                            MonadUnliftIO m,
                            HasLogFunc env) => UserConfig -> m (Maybe (Vector GH.Repo))
getReposForUserGithubCom userConfig = do
  --TODO: Config for this needs some rework
  -- currently, we allow a `None Text` in the config file here, which doesn't make sense because:
  -- currentUserReposR requires a token
  -- Should probably:
  -- If we have a token, use currentUserReposR - we don't need a name in this case
  -- If no token, use userReposR - we do need a name in this case
  possibleRepos <- liftIO $ GH.github
                            (GH.OAuth (fromString $ fromMaybe "" $ userAPIToken userConfig))
                            GH.currentUserReposR GH.RepoPublicityAll GH.FetchAll
  case possibleRepos of
    (Left err) -> do
      logInfo $ displayShow err
      return Nothing
    (Right repos) -> do
      return $ Just repos

getReposForUserConfig :: (MonadReader env m,
                         MonadUnliftIO m,
                         HasLogFunc env) => UserConfig -> m (Maybe (Vector GH.Repo), UserConfig)
getReposForUserConfig userConfig = do
    repos <- getReposForUserGithubCom userConfig
    return (repos, userConfig)

filterRepos :: UserConfig -> Vector GH.Repo -> Vector GH.Repo
filterRepos userConfig = do
  V.filter (\r ->
    -- Forks
    GH.repoFork r /= Just True
    -- Ignore list in config
    && notElem (unpack (GH.untagName $ GH.repoName r)) (ignoringUserRepos userConfig)
   )

-- TODO: Allow this to be overridden in config at some point
-- Suffix for a repo, append this to the rootDir
dirSuffixForRepo :: UserConfig -> GH.Repo -> FilePath
dirSuffixForRepo userConfig repo = do
  unpack $ GH.untagName (GH.simpleOwnerLogin $ GH.repoOwner repo)

cloneSingleUserConfig :: ( MonadReader env m,
                          MonadUnliftIO m,
                          HasLogFunc env) => FilePath -> Maybe (Vector GH.Repo) -> UserConfig -> m ()
cloneSingleUserConfig rootDir maybeRepos userConfig = do

  case maybeRepos of
    Nothing -> return ()
    Just repos'' -> do
      let repos = filterRepos userConfig repos''
      logInfo $ "Cloning '" <> displayShow (userName userConfig) <> "' repos. There are: " <> displayShow (length repos)

      let indexedRepos = getZipSource $ (,)
                            <$> ZipSource (yieldMany ([1..] :: [Int]))
                            <*> ZipSource (CL.sourceList (toList repos))

      res <- runConduitRes
                 $ indexedRepos
                .| mapMC (\(idx, result) -> do
                    logSticky $ "Cloning " <> displayShow idx <> "/" <> displayShow (length repos)
                    return result
                  )
                .| concurrentMapM_ 8 10 (\repo -> do
                    let cloneIntoDir = rootDir </> userName userConfig </> dirSuffixForRepo userConfig repo
                    cloneSingleRepo cloneIntoDir repo
                )
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

      logStickyDone ""
      return ()

-- for orgs in config, get repos, pick first one for now
-- if the dir doesn't exist, clone the repo, otherwise warn and skip
cloneUserConfigs :: (MonadReader env m,
                 MonadUnliftIO m,
                 HasLogFunc env) => FilePath -> Vector UserConfig -> m ()
cloneUserConfigs topDir userConfigs' = do
  -- First, we want to request all the repos upfront from all configs
  let indexedConfigs = getZipSource $ (,)
                          <$> ZipSource (yieldMany ([1..] :: [Int]))
                          <*> ZipSource (CL.sourceList (toList userConfigs'))
  _ <- runConduitRes
              $ indexedConfigs
              -- Log sticky
              .| mapMC (\(idx, userConfig) -> do
                  logSticky $ "Fetching Config Repos " <> displayShow idx <> "/" <> displayShow (length userConfigs')
                  return userConfig
                )
              -- Fetch list of repos
              .| concurrentMapM_ 8 10 getReposForUserConfig
              -- Each thing coming through here should be (Maybe (Vector Repo), OrgConfig)
              -- So now we should just be able to hand this off to cloneSingleOrgConfig. Run one at a time, but internally concurrent.
              .| mapMC (\(maybeRepos, userConfig) -> do
                  cloneSingleUserConfig topDir maybeRepos userConfig
                )
              .| CL.consume

  logSticky "All Done!!"

