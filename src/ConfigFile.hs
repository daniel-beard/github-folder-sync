{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module ConfigFile (configFileName, getConfig) where

import Data.Maybe
import Dhall
import Import
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import RIO.Lens
import RIO.List
import qualified RIO.Text as T
import System.Directory
import System.Environment
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

configFileName :: FilePath
configFileName = ".github-folder-sync"

getConfig :: IO (Maybe Config)
getConfig = do
    maybeConfigFile <- findConfigFile
    case maybeConfigFile of
        Nothing -> return Nothing
        Just file -> do
          let configDirectory = takeDirectory  file
          rawConfig <- detailed $ input auto $ T.pack file
          environment <- getEnvironment
          -- Expand env variables
          let transformedConfig = rawConfig 
                      & (orgConfigsL . traverse . githubAPITokenL . _Just %~ expandEnvVar environment)
                      & (orgConfigsL . traverse . githubAPIEndpointL . _Just %~ expandEnvVar environment)
                      & ((orgConfigsL . traverse . orgNameL) %~ expandEnvVar environment)
          return $ Just Config { configFile = transformedConfig, topLevelDir = configDirectory }

-- Config file has 'package.json'-like resolution rules.
-- i.e. we traverse up directories until we find a config file, starting from working directory
findConfigFile :: (MonadUnliftIO m) => m (Maybe FilePath)
findConfigFile = do
  currDir <- liftIO getCurrentDirectory
  path' <- liftIO $ canonicalizePath currDir
  let mkPaths p
        | all isPathSeparator p || p == "." = []
        | otherwise = (p </> configFileName) : mkPaths (takeDirectory p)
  liftIO $ firstMaybeConfigFile $ mkPaths path'
  where firstMaybeConfigFile :: [FilePath] -> IO (Maybe FilePath)
        firstMaybeConfigFile [] = return Nothing
        firstMaybeConfigFile [x] = do
          exists <- doesFileExist x
          if exists then return $ Just x else return Nothing
        firstMaybeConfigFile (x:xs) = do
          exists <- doesFileExist x
          if exists then return $ Just x else firstMaybeConfigFile xs