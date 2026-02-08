{-# LANGUAGE OverloadedStrings #-}

module NixRebuildAnalyser
  ( nixRebuildAnalyserMain,
  )
where

import Control.Monad (when)
import Control.Monad.Logger (LoggingT, filterLogger, logErrorN, logInfoN, logWarnN, runStderrLoggingT)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as Text
import NixRebuildAnalyser.Analyse (AnalysisResult (..), analyseAll, getGitFiles)
import NixRebuildAnalyser.FlakeShow (FlakeOutput (..), discoverFlakeOutputs)
import NixRebuildAnalyser.OptParse (Settings (..), getSettings)
import NixRebuildAnalyser.Output (outputAnalysisResult)
import Path (Abs, Dir, File, Path, Rel, parseRelFile, toFilePath)
import Path.IO (getCurrentDir)
import System.Exit (exitFailure)
import Text.Colour.Term (putChunksLocale)
import UnliftIO (liftIO)

nixRebuildAnalyserMain :: IO ()
nixRebuildAnalyserMain = do
  settings <- getSettings
  flakeDir <- getCurrentDir
  let logLevel = settingLogLevel settings
      logFilter _ level = level >= logLevel
  runStderrLoggingT $ filterLogger logFilter $ do
    result <- runAnalysis settings flakeDir
    case result of
      Left err -> do
        logErrorN $ Text.pack err
        liftIO exitFailure
      Right analysisResult -> do
        liftIO $ printResults settings analysisResult

runAnalysis :: Settings -> Path Abs Dir -> LoggingT IO (Either String AnalysisResult)
runAnalysis settings flakeDir = do
  logInfoN $
    Text.pack $
      unwords
        [ "Analysing flake in:",
          toFilePath flakeDir
        ]

  -- Discover flake outputs
  outputsE <- discoverFlakeOutputs flakeDir
  case outputsE of
    Left err -> pure $ Left err
    Right allOutputs -> do
      if null allOutputs
        then pure $ Left "No buildable outputs found in flake"
        else do
          logInfoN $
            Text.pack $
              unwords
                [ "Found",
                  show (length allOutputs),
                  "outputs"
                ]

          -- Filter outputs if --output filters specified
          let outputFilters = settingOutputFilters settings
              outputs =
                if null outputFilters
                  then allOutputs
                  else filter (\o -> flakeOutputAttribute o `elem` outputFilters) allOutputs

          -- Warn about unmatched output filters
          let matchedAttrs = Set.fromList $ map flakeOutputAttribute outputs
              unmatchedFilters = filter (`Set.notMember` matchedAttrs) outputFilters
          mapM_ (\f -> logWarnN $ "Output filter does not match any output: " <> f) unmatchedFilters

          if null outputs
            then pure $ Left "No outputs match the specified filters"
            else do
              when (not $ null outputFilters) $
                logInfoN $
                  Text.pack $
                    unwords
                      [ "Analysing",
                        show (length outputs),
                        "filtered outputs"
                      ]

              -- Get git files
              filesE <- getGitFiles flakeDir
              case filesE of
                Left err -> pure $ Left err
                Right files -> do
                  if null files
                    then pure $ Left "No git-tracked files found"
                    else do
                      logInfoN $
                        Text.pack $
                          unwords
                            [ "Found",
                              show (length files),
                              "files"
                            ]

                      -- Run analysis
                      result <- analyseAll flakeDir outputs files
                      pure $ Right result

printResults :: Settings -> AnalysisResult -> IO ()
printResults settings result =
  let fileFilters = mapMaybe parseRelFile (settingFileFilters settings)
      filteredResult = filterResultByFiles fileFilters result
   in if settingJson settings
        then LB.putStrLn $ Aeson.encode filteredResult
        else mapM_ putChunksLocale (outputAnalysisResult settings filteredResult)

-- | Filter the analysis result to only include specified files
filterResultByFiles :: [Path Rel File] -> AnalysisResult -> AnalysisResult
filterResultByFiles [] result = result
filterResultByFiles fileFilters result =
  let fileSet = Set.fromList fileFilters
      filteredFileToOutputs = Map.filterWithKey (\k _ -> k `Set.member` fileSet) (resultFileToOutputs result)
      -- Also filter outputToFiles to only show outputs that are affected by the filtered files
      relevantOutputs = Set.unions $ Map.elems filteredFileToOutputs
      filteredOutputToFiles = Map.filterWithKey (\k _ -> k `Set.member` relevantOutputs) (resultOutputToFiles result)
   in AnalysisResult
        { resultFileToOutputs = filteredFileToOutputs,
          resultOutputToFiles = Map.map (Set.intersection fileSet) filteredOutputToFiles
        }
