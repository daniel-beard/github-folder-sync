{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module ConfigFile (defaultConfigFileName, getConfig) where

import Data.Maybe
import Dhall
import Import
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import RIO.Lens
import RIO.List
import System.Directory
import System.Environment ( getEnvironment )
import System.FilePath

-- Config File Env Var Parsing
-----------------------------------------------------------------------------------

type Parser = M.Parsec Void String

data ConfigEntry = VarEntry String | LiteralEntry String

-- Supports '$VARNAME' or a literal value, since Dhall uses '${VARNAME}' pattern
pConfigEntry :: Parser ConfigEntry
pConfigEntry = VarEntry <$> pEnvVar
           <|> LiteralEntry <$> M.some L.charLiteral
  where pEnvVar :: Parser String
        pEnvVar = C.char '$' *>  M.some L.charLiteral

expandEnvVar :: [(String, String)] -> String -> String
expandEnvVar environment inputString = do
    case M.parse pConfigEntry "" inputString of
        Left _ -> ""
        Right val -> case val of
          VarEntry v -> case headMaybe $ filter ((== v) . fst) environment of
              Nothing -> error $ "Env var not found for: " <> v
              Just l -> snd l
          LiteralEntry l -> l

defaultConfigFileName :: FilePath
defaultConfigFileName = ".github-folder-sync"

workingDirConfigFilePath :: IO FilePath
workingDirConfigFilePath = do
  pwd <- getCurrentDirectory
  return $ pwd </> defaultConfigFileName

-- Config path resolution is either:
-- - A passed in app option
-- - .github-folder-sync in the current directory
-- In that order, we use the first that exists
getConfig :: Maybe FilePath -> IO (Maybe Config)
getConfig configFileAppOption = do
    configFileInPWD <- workingDirConfigFilePath
    configFilesThatExist <- filterM doesFileExist $ catMaybes [configFileAppOption <|> Just configFileInPWD]
    let firstConfigFileThatExists = listToMaybe configFilesThatExist

    case firstConfigFileThatExists of
        Nothing -> return Nothing
        Just file -> do
          let dhallTypeDefs = dhallOrgConfigType <> "\n" <> dhallUserConfigType <> "\n"
          fileContent <- readFileUtf8 file
          rawConfig <- detailed $ input auto $ dhallTypeDefs <> fileContent
          environment <- getEnvironment

          -- Expand env variables
          let eachOrgConfigL = orgConfigsL . traverse
          let eachUserConfigL = userConfigsL . traverse
          let transformedConfig = rawConfig 
                      & (eachOrgConfigL . orgAPITokenL . _Just %~ expandEnvVar environment)
                      & (eachOrgConfigL . orgNameL %~ expandEnvVar environment)
                      & (eachUserConfigL . userNameL %~ expandEnvVar environment)
                      & (eachUserConfigL . userAPITokenL . _Just %~ expandEnvVar environment)
          return $ Just Config { configFile = transformedConfig, configFilePath = file }

