{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module GithubShared (cloneSingleRepo, ghClient) where

import Conduit
import qualified Data.Conduit.Process.Typed as PT
import qualified Data.Conduit.List as CL
import qualified GitHub as GH
import RIO
import RIO.Process
import RIO.Text hiding (length, filter, map)
import RIO.Directory
import System.FilePath

-- Returns a read-only or read-write GH client based on if a token is present
ghClient :: (GH.GitHubRO req res, GH.GitHubRW req res) => Maybe String -> req -> res
ghClient maybeToken = do
  case maybeToken of
    Nothing -> GH.github'
    Just token -> GH.github (GH.OAuth $ fromString token)

cloneSingleRepo :: (MonadReader env m,
                    MonadUnliftIO m,
                    HasLogFunc env) => FilePath -> GH.Repo -> m ([ByteString], ExitCode)
cloneSingleRepo intoDir repo = do
  -- Check if the repo folder exists, if not clone it
  let repoFolder = intoDir </> unpack (GH.untagName $ GH.repoName repo)
  createDirectoryIfMissing True repoFolder
  let repoGitFolder = repoFolder </> ".git"
  repoGitFolderExists <- doesPathExist repoGitFolder
  let cloneSshURL = GH.repoSshUrl repo

  if repoGitFolderExists then do
    logInfo $ "Skipping: looks like there's already a git repo at: " <> displayShow repoGitFolder
    return ([], ExitSuccess)
  else
    case cloneSshURL of
      Nothing -> return ([], ExitSuccess)
      Just sshURL -> do
        logInfo $ "Running: 'git clone " <> displayShow (unpack (GH.getUrl sshURL)) <> "'"
        let pc = setStderr PT.createSource $ PT.proc "git" ["clone", unpack (GH.getUrl sshURL), repoFolder]
        withProcessWait pc $ \p ->
          runConduit (getStderr p .| CL.consume) `concurrently`
          runConduit (waitExitCode p)
