{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import ConfigFile (defaultConfigFileName)
import Import
import Options.Applicative.Simple
import Run
import RIO.Process
import qualified Paths_github_folder_sync

optVerbose :: Parser Bool
optVerbose = switch ( long "verbose"
                  <> short 'v'
                  <> help "Verbose output?"
                  )

optConfigFile :: Parser (Maybe String)
optConfigFile = optional $ strOption ( long "config-file"
                  <> short 'c'
                  <> metavar "CONFIGFILE"
                  <> help ("Specify a CONFIGFILE path. Default is ./" <> defaultConfigFileName)
                  )

optOutputDir :: Parser (Maybe String)
optOutputDir = optional $ strOption ( long "output-dir"
                  <> short 'o'
                  <> metavar "OUTPUTDIR"
                  <> help "Specify an output directory. Default is PWD"
                  )

main :: IO ()
main = do

  (options, ()) <- simpleOptions
    $(simpleVersion Paths_github_folder_sync.version)
    "github-folder-sync"
    "Sync repos and orgs from github to folder structures"
    (Options <$> optVerbose <*> optConfigFile <*> optOutputDir
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
