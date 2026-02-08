{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module NixRebuildAnalyser.OptParse
  ( Settings (..),
    getSettings,
  )
where

import Control.Monad.Logger (LogLevel (..))
import Data.Text (Text)
import OptEnvConf
import Paths_nix_rebuild_analyser (version)

data Settings = Settings
  { settingLogLevel :: LogLevel,
    settingFileFilters :: [FilePath],
    settingOutputFilters :: [Text],
    settingJson :: Bool
  }
  deriving (Show)

getSettings :: IO Settings
getSettings = runSettingsParser version "Analyse which files affect which Nix flake outputs"

instance HasParser Settings where
  settingsParser = do
    settingLogLevel <-
      setting
        [ help "Log level (Debug, Info, Warn, Error)",
          reader logLevelReader,
          option,
          long "log-level",
          metavar "LEVEL",
          value LevelInfo
        ]
    settingFileFilters <-
      many $
        setting
          [ help "Filter to specific file(s) - show which outputs they affect",
            reader str,
            option,
            long "file",
            metavar "PATH"
          ]
    settingOutputFilters <-
      many $
        setting
          [ help "Filter to specific output(s) - show which files affect them",
            reader str,
            option,
            long "output",
            metavar "ATTRIBUTE"
          ]
    settingJson <-
      setting
        [ help "Output results as JSON",
          switch True,
          value False,
          long "json"
        ]
    pure Settings {..}

logLevelReader :: Reader LogLevel
logLevelReader = eitherReader $ \s ->
  case s of
    "Debug" -> Right LevelDebug
    "Info" -> Right LevelInfo
    "Warn" -> Right LevelWarn
    "Error" -> Right LevelError
    _ -> Left $ "Unknown log level: " <> s <> ". Expected: Debug, Info, Warn, Error"
