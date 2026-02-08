{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module NixRebuildAnalyser.Analyse
  ( AnalysisResult (..),
    analyseAll,
    getGitFiles,
  )
where

import Control.Monad.Logger (LoggingT, logDebugN, logInfoN)
import Data.Aeson (ToJSON (..), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LB
import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import NixRebuildAnalyser.FlakeShow (FlakeOutput (..))
import Path (Abs, Dir, File, Path, Rel, parseAbsDir, parseRelFile, toFilePath)
import Path.IO (doesDirExist, listDirRecur)
import System.Exit (ExitCode (..))
import System.Process.Typed (proc, readProcess, setWorkingDir)
import UnliftIO (liftIO, pooledMapConcurrently)

data AnalysisResult = AnalysisResult
  { resultFileToOutputs :: !(Map (Path Rel File) (Set FlakeOutput)),
    resultOutputToFiles :: !(Map FlakeOutput (Set (Path Rel File)))
  }
  deriving (Show, Eq)

instance ToJSON AnalysisResult where
  toJSON result =
    Aeson.object
      [ "fileToOutputs" .= fileToOutputsJson,
        "outputToFiles" .= outputToFilesJson
      ]
    where
      fileToOutputsJson =
        Map.fromList
          [ (Text.pack (toFilePath f), Set.map flakeOutputAttribute os)
          | (f, os) <- Map.toList (resultFileToOutputs result)
          ]
      outputToFilesJson =
        Map.fromList
          [ (flakeOutputAttribute o, Set.map (Text.pack . toFilePath) fs)
          | (o, fs) <- Map.toList (resultOutputToFiles result)
          ]

getGitFiles :: Path Abs Dir -> LoggingT IO (Either String [Path Rel File])
getGitFiles flakeDir = do
  logInfoN "Getting git-tracked files..."
  let processConfig =
        setWorkingDir (toFilePath flakeDir) $
          proc "git" ["ls-files"]
  (exitCode, stdout, _stderr) <- liftIO $ readProcess processConfig
  case exitCode of
    ExitFailure code ->
      pure $ Left $ concat ["git ls-files failed with code ", show code]
    ExitSuccess -> do
      let stdoutText = Text.decodeUtf8 $ LB.toStrict stdout
          fileLines = filter (not . Text.null) $ Text.lines stdoutText
      files <- liftIO $ mapM (parseRelFile . Text.unpack) fileLines
      logDebugN $
        Text.pack $
          unwords
            [ "Found",
              show (length files),
              "git-tracked files"
            ]
      pure $ Right files

-- | A source file paired with the name of the store path it came from.
-- The store path name is used to match against git file prefixes.
data SourceFile = SourceFile
  { -- | The name portion of the store path (e.g., "nix-ci-blog" from "/nix/store/xxx-nix-ci-blog")
    sourceFileStorePathName :: !Text,
    -- | The relative path within the store path
    sourceFileRelPath :: !(Path Rel File)
  }
  deriving (Show, Eq, Ord)

-- | Analyse which files affect which outputs using static derivation analysis.
-- This parses `nix derivation show --recursive` to find source store paths
-- for each output, then lists files in those paths.
analyseAll ::
  Path Abs Dir ->
  [FlakeOutput] ->
  [Path Rel File] ->
  LoggingT IO AnalysisResult
analyseAll flakeDir outputs files = do
  logInfoN "Analysing derivation dependencies (static analysis)..."

  -- For each output, find its source store paths and list files (in parallel)
  outputToSourceFilesMap <-
    fmap Map.fromList $
      pooledMapConcurrently
        ( \output -> do
            logInfoN $ "Analysing output: " <> flakeOutputAttribute output
            sourceFiles <- getOutputSourceFiles flakeDir output
            logDebugN $ Text.pack $ unwords ["  Found", show (Set.size sourceFiles), "source files"]
            pure (output, sourceFiles)
        )
        outputs

  -- Match git files to source files and build mappings
  let gitFileSet = Set.fromList files
      (fileToOutputsMap, matchedOutputToFiles) = buildFileToOutputs gitFileSet outputToSourceFilesMap

  pure
    AnalysisResult
      { resultFileToOutputs = fileToOutputsMap,
        resultOutputToFiles = matchedOutputToFiles
      }

-- | Get source files that affect a specific output by parsing derivation info
getOutputSourceFiles ::
  Path Abs Dir ->
  FlakeOutput ->
  LoggingT IO (Set SourceFile)
getOutputSourceFiles flakeDir output = do
  let attr = ".#" <> Text.unpack (flakeOutputAttribute output)
      processConfig =
        setWorkingDir (toFilePath flakeDir) $
          proc "nix" ["derivation", "show", "--recursive", attr]
  (exitCode, stdout, _stderr) <- liftIO $ readProcess processConfig
  case exitCode of
    ExitFailure _ -> pure Set.empty
    ExitSuccess -> do
      -- Parse the JSON to extract inputSrcs
      case Aeson.decode stdout of
        Just (Aeson.Object drvs) -> do
          let allInputSrcs = concatMap extractInputSrcs $ KeyMap.elems drvs
              -- Filter to source directories (not builder scripts)
              sourcePaths = filter isSourcePath allInputSrcs
          -- List files in each source path and convert to relative paths
          allFiles <- concat <$> mapM listSourceFilesWithStoreName sourcePaths
          pure $ Set.fromList allFiles
        _ -> pure Set.empty

-- | Extract inputSrcs from a derivation JSON object
extractInputSrcs :: Aeson.Value -> [Text]
extractInputSrcs (Aeson.Object obj) =
  case KeyMap.lookup "inputSrcs" obj of
    Just (Aeson.Array arr) ->
      [t | Aeson.String t <- foldr (:) [] arr]
    _ -> []
extractInputSrcs _ = []

-- | Check if a store path looks like a source directory (not a builder script)
isSourcePath :: Text -> Bool
isSourcePath path =
  not (any (`Text.isInfixOf` path) excludePatterns)
  where
    excludePatterns =
      [ "source-stdenv",
        "default-builder",
        "Setup.hs",
        "builder.sh",
        "-hook",
        "-wrapper",
        "setup-hook",
        ".patch",
        ".diff",
        ".sh",
        ".awk",
        ".bash",
        "bootstrap",
        ".tar.gz",
        ".cabal"
      ]

-- | Extract the name portion from a Nix store path.
-- E.g., "/nix/store/abc123-nix-ci-blog" -> "nix-ci-blog"
extractStorePathName :: Text -> Text
extractStorePathName storePathText =
  case Text.splitOn "/" storePathText of
    -- /nix/store/hash-name -> ["", "nix", "store", "hash-name"]
    (_empty : _nix : _store : hashName : _rest) ->
      -- Strip the hash prefix (everything before and including the first dash)
      case Text.breakOn "-" hashName of
        (_, rest) | not (Text.null rest) -> Text.drop 1 rest -- drop the leading dash
        _ -> hashName -- no dash found, use as-is
    _ -> storePathText -- fallback to full path

-- | List files in a source store path, returning SourceFile with the store path name
listSourceFilesWithStoreName :: Text -> LoggingT IO [SourceFile]
listSourceFilesWithStoreName storePathText = do
  let storePath = Text.unpack storePathText
      storePathName = extractStorePathName storePathText
  case parseAbsDir (storePath ++ "/") of
    Nothing -> pure []
    Just absDir -> do
      isDir <- liftIO $ doesDirExist absDir
      if isDir
        then do
          (_, files) <- liftIO $ listDirRecur absDir
          -- Convert to relative paths by stripping the store path prefix
          let relPaths = mapMaybe (toRelativePath absDir) files
          pure [SourceFile storePathName p | p <- relPaths]
        else pure []

-- | Convert an absolute file path to a relative path by stripping the store dir prefix
toRelativePath :: Path Abs Dir -> Path Abs File -> Maybe (Path Rel File)
toRelativePath storeDir filePath =
  let storeDirStr = toFilePath storeDir
      filePathStr = toFilePath filePath
   in if storeDirStr `isPrefixOf` filePathStr
        then parseRelFile (drop (length storeDirStr) filePathStr)
        else Nothing

-- | Build the file -> outputs mapping from the output -> files mapping
-- Also returns a filtered outputToFiles map that only includes git-tracked files
buildFileToOutputs ::
  Set (Path Rel File) ->
  Map FlakeOutput (Set SourceFile) ->
  (Map (Path Rel File) (Set FlakeOutput), Map FlakeOutput (Set (Path Rel File)))
buildFileToOutputs gitFiles outputToSourceFiles =
  let -- For each output, find which git files match its source files
      matchedOutputToFiles = Map.map (matchGitFiles gitFiles) outputToSourceFiles
      fileToOutputs =
        Map.fromListWith
          Set.union
          [ (file, Set.singleton output)
          | (output, files) <- Map.toList matchedOutputToFiles,
            file <- Set.toList files
          ]
   in (fileToOutputs, matchedOutputToFiles)

-- | Find git files that match the given source files.
--
-- A git file matches a source file if:
-- 1. The git file path starts with the store path name (e.g., "nix-ci-blog/")
-- 2. The remainder of the git file path equals the source file's relative path
--
-- For example:
-- - SourceFile "nix-ci-blog" "test/Spec.hs" matches "nix-ci-blog/test/Spec.hs"
-- - SourceFile "validity" "test/Spec.hs" does NOT match "nix-ci-blog/test/Spec.hs"
matchGitFiles :: Set (Path Rel File) -> Set SourceFile -> Set (Path Rel File)
matchGitFiles gitFiles sourceFiles =
  Set.fromList
    [ gitFile
    | gitFile <- Set.toList gitFiles,
      sourceFile <- Set.toList sourceFiles,
      matchesSourceFile gitFile sourceFile
    ]

-- | Check if a git file matches a source file.
-- The git file must start with the store path name followed by the source file's relative path.
matchesSourceFile :: Path Rel File -> SourceFile -> Bool
matchesSourceFile gitFile (SourceFile storePathName sourceRelPath) =
  let gitFilePath = toFilePath gitFile
      expectedPrefix = Text.unpack storePathName ++ "/"
      expectedPath = expectedPrefix ++ toFilePath sourceRelPath
   in gitFilePath == expectedPath
