{-# LANGUAGE ScopedTypeVariables #-}

module NixRebuildAnalyser.E2ESpec (spec) where

import Control.Monad.Logger (LogLevel (..), filterLogger, runStderrLoggingT)
import Data.Aeson (Value, eitherDecodeStrict)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import NixRebuildAnalyser (AnalysisResult, Settings (..), runAnalysis)
import Path (Abs, Dir, Path, parseRelDir, toFilePath, (</>))
import Path.IO (copyDirRecur, resolveDir', withSystemTempDir)
import System.Exit (ExitCode (..))
import System.Process.Typed (proc, readProcess, setWorkingDir)
import Test.Syd

spec :: Spec
spec = do
  describe "nix-rebuild-analyser E2E" $ do
    it "analyzes a simple flake" $ do
      goldenByteStringFile "test_resources/golden/simple-flake.json" $ do
        runAnalyserOnFixture "simple-flake"

    it "analyzes a multi-output flake" $ do
      goldenByteStringFile "test_resources/golden/multi-output-flake.json" $ do
        runAnalyserOnFixture "multi-output-flake"

    it "analyzes a flake with same-named files in different directories" $ do
      goldenByteStringFile "test_resources/golden/same-name-files.json" $ do
        runAnalyserOnFixture "same-name-files"

    it "analyzes a flake with same path under different store paths" $ do
      goldenByteStringFile "test_resources/golden/same-path-different-store.json" $ do
        runAnalyserOnFixture "same-path-different-store"

-- | Run the analyser on a test fixture and return normalized JSON output
runAnalyserOnFixture :: String -> IO BS.ByteString
runAnalyserOnFixture fixtureName = do
  withSystemTempDir "nix-rebuild-analyser-test" $ \tmpDir -> do
    -- Copy fixture to temp dir
    fixtureDir <- resolveDir' ("test_resources/fixtures/" ++ fixtureName)
    let destDir = tmpDir </> fixtureName'
        fixtureName' = case parseRelDir fixtureName of
          Just d -> d
          Nothing -> error $ "Invalid fixture name: " ++ fixtureName

    copyDirRecur fixtureDir destDir

    -- Initialize git repo (required by the tool)
    initGitRepo destDir

    -- Run nix-rebuild-analyser
    result <- runAnalyser destDir

    -- Normalize and return
    pure $ normalizeJsonOutput result

-- | Initialize a git repository in the given directory
initGitRepo :: Path Abs Dir -> IO ()
initGitRepo dir = do
  let runGit args = do
        (exitCode, _, stderr) <-
          readProcess $
            setWorkingDir (toFilePath dir) $
              proc "git" args
        case exitCode of
          ExitSuccess -> pure ()
          ExitFailure code ->
            error $ "git " ++ unwords args ++ " failed with code " ++ show code ++ ": " ++ show stderr

  runGit ["init", "-q"]
  runGit ["config", "user.email", "test@test.com"]
  runGit ["config", "user.name", "Test"]
  runGit ["add", "."]
  runGit ["commit", "-q", "-m", "initial"]

-- | Run nix-rebuild-analyser library in the given directory
runAnalyser :: Path Abs Dir -> IO AnalysisResult
runAnalyser dir = do
  let settings =
        Settings
          { settingLogLevel = LevelWarn,
            settingFileFilters = [],
            settingOutputFilters = [],
            settingJson = True
          }
      logFilter _ level = level >= LevelWarn
  resultE <- runStderrLoggingT $ filterLogger logFilter $ runAnalysis settings dir
  case resultE of
    Left err -> error $ "Analysis failed: " ++ err
    Right result -> pure result

-- | Normalize JSON output for deterministic comparison
-- This parses the JSON and re-encodes it with sorted keys
normalizeJsonOutput :: AnalysisResult -> BS.ByteString
normalizeJsonOutput result =
  let encoded = LB.toStrict $ encodePretty result
   in case eitherDecodeStrict encoded of
        Left err -> error $ "Failed to parse JSON output: " ++ err
        Right (val :: Value) -> LB.toStrict $ encodePretty val
