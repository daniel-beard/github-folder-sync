{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Import
import Options.Applicative.Simple
import Run
import RIO.Process
import System.Directory
import System.FilePath

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
                  -- <*> strOption ( long "org"
                  -- <> metavar "GITHUB_ORG"
                  -- <> help "The github.com org to sync"
                  -- )
    )
    empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          }
     in runRIO app run
