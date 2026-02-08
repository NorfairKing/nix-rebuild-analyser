{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module NixRebuildAnalyser.Output
  ( outputAnalysisResult,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import NixRebuildAnalyser.Analyse (AnalysisResult (..))
import NixRebuildAnalyser.FlakeShow (FlakeOutput (..))
import NixRebuildAnalyser.OptParse (Settings (..))
import Path (toFilePath)
import Text.Colour (Chunk, bold, chunk, fore, green, yellow)

outputAnalysisResult :: Settings -> AnalysisResult -> [[Chunk]]
outputAnalysisResult settings AnalysisResult {..} =
  let hasFileFilter = not $ null $ settingFileFilters settings
      hasOutputFilter = not $ null $ settingOutputFilters settings
   in if Map.null resultOutputToFiles
        then
          [ [bold $ chunk "\n=== Analysis Results ===\n\n"],
            [fore yellow $ chunk "No files affect any outputs.\n"]
          ]
        else
          concat
            [ [[bold $ chunk "\n=== Analysis Results ===\n\n"]],
              -- Files affecting each output (hide if file filter applied)
              if hasFileFilter
                then []
                else
                  [[bold $ chunk "Files affecting each output:\n\n"]]
                    ++ concatMap outputForOutput (Map.toList resultOutputToFiles),
              -- Outputs affected by each file (hide if output filter applied)
              if hasOutputFilter
                then []
                else
                  [[bold $ chunk "Outputs affected by each file:\n\n"]]
                    ++ concatMap outputForFile (Map.toList resultFileToOutputs),
              -- Summary
              [summaryLine]
            ]
  where
    outputForOutput (output, files) =
      concat
        [ [ [ fore green $ bold $ chunk $ flakeOutputAttribute output,
              chunk " (",
              chunk $ Text.pack $ show (Set.size files),
              chunk " files)\n"
            ]
          ],
          map fileEntry (Set.toList files),
          [[chunk "\n"]]
        ]

    fileEntry file =
      [ chunk "  - ",
        chunk $ Text.pack $ toFilePath file,
        chunk "\n"
      ]

    outputForFile (file, outputs) =
      concat
        [ [ [ fore green $ bold $ chunk $ Text.pack $ toFilePath file,
              chunk " (",
              chunk $ Text.pack $ show (Set.size outputs),
              chunk " outputs)\n"
            ]
          ],
          map outputEntry (Set.toList outputs),
          [[chunk "\n"]]
        ]

    outputEntry output =
      [ chunk "  - ",
        chunk $ flakeOutputAttribute output,
        chunk "\n"
      ]

    summaryLine =
      let totalOutputs = Map.size resultOutputToFiles
          totalFiles = Map.size resultFileToOutputs
       in [ bold $ chunk "Summary: ",
            chunk $ Text.pack $ show totalFiles,
            chunk " files affect ",
            chunk $ Text.pack $ show totalOutputs,
            chunk " outputs.\n"
          ]
