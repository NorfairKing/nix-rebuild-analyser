{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module NixRebuildAnalyser.FlakeShow
  ( FlakeOutput (..),
    discoverFlakeOutputs,
  )
where

import Control.Monad.Logger (LoggingT, logDebugN, logInfoN)
import Data.Aeson (ToJSON (..), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Key (toText)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as Text
import Path (Abs, Dir, Path, toFilePath)
import System.Exit (ExitCode (..))
import System.Process.Typed (proc, readProcess, setWorkingDir)
import UnliftIO (liftIO)

data FlakeOutput = FlakeOutput
  { flakeOutputArch :: !Text,
    flakeOutputName :: !Text,
    flakeOutputAttribute :: !Text
  }
  deriving (Show, Eq, Ord)

instance ToJSON FlakeOutput where
  toJSON fo =
    Aeson.object
      [ "arch" .= flakeOutputArch fo,
        "name" .= flakeOutputName fo,
        "attribute" .= flakeOutputAttribute fo
      ]

discoverFlakeOutputs :: Path Abs Dir -> LoggingT IO (Either String [FlakeOutput])
discoverFlakeOutputs flakeDir = do
  logInfoN "Discovering flake outputs..."
  let processConfig =
        setWorkingDir (toFilePath flakeDir) $
          proc "nix" ["flake", "show", "--json"]
  (exitCode, stdout, stderr) <- liftIO $ readProcess processConfig
  case exitCode of
    ExitFailure code -> do
      let errMsg = concat ["nix flake show failed with code ", show code, ": ", show stderr]
      pure $ Left errMsg
    ExitSuccess -> do
      case Aeson.eitherDecode stdout of
        Left err -> pure $ Left $ "Failed to parse nix flake show output: " <> err
        Right val -> do
          let outputs = parseFlakeShowValue val
          logDebugN $ Text.pack $ unwords ["Found", show (length outputs), "outputs"]
          pure $ Right outputs

parseFlakeShowValue :: Aeson.Value -> [FlakeOutput]
parseFlakeShowValue = \case
  Aeson.Object o ->
    concat
      [ parseCategory "packages" o,
        parseCategory "checks" o,
        parseCategory "devShells" o,
        parseCategory "apps" o,
        parseFormatter o
      ]
  _ -> []

parseCategory :: Text -> Aeson.Object -> [FlakeOutput]
parseCategory category o =
  case KeyMap.lookup (Key.fromText category) o of
    Just (Aeson.Object archMap) ->
      [ FlakeOutput arch name (category <> "." <> arch <> "." <> name)
      | (archKey, archVal) <- KeyMap.toList archMap,
        let arch = toText archKey,
        Aeson.Object nameMap <- [archVal],
        (nameKey, nameVal) <- KeyMap.toList nameMap,
        let name = toText nameKey,
        isDerivation nameVal
      ]
    _ -> []

parseFormatter :: Aeson.Object -> [FlakeOutput]
parseFormatter o =
  case KeyMap.lookup "formatter" o of
    Just (Aeson.Object archMap) ->
      [ FlakeOutput arch "formatter" ("formatter." <> arch)
      | (archKey, archVal) <- KeyMap.toList archMap,
        let arch = toText archKey,
        isDerivation archVal
      ]
    _ -> []

isDerivation :: Aeson.Value -> Bool
isDerivation = \case
  Aeson.Object o ->
    case KeyMap.lookup "type" o of
      Just (Aeson.String "derivation") -> True
      _ -> False
  _ -> False
