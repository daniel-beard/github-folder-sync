{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Import
import Options.Applicative.Simple
import ConfigFile
import Run
import RIO.Process
import qualified Paths_github_folder_sync

main :: IO ()
main = do

  (options, ()) <- simpleOptions
    $(simpleVersion Paths_github_folder_sync.version)
    "github-folder-sync"
    "Sync repos and orgs from github to folder structures"
    (Options <$> switch ( long "verbose"
                 <> short 'v'
                 <> help "Verbose output?"
                  )
    )
    empty
  maybeConfig <- getConfig 
  case maybeConfig of
    Nothing -> error $ "Could not find config file. Make sure you have '" ++ configFileName ++ "' in the current dir, or any parent dir."
    Just config -> do
      lo <- logOptionsHandle stderr (optionsVerbose options)
      pc <- mkDefaultProcessContext
      withLogFunc lo $ \lf ->
        let app = App
              { appLogFunc = lf
              , appProcessContext = pc
              , appOptions = options
              , config = config
              }
        in runRIO app run
