{-# LANGUAGE OverloadedStrings #-}

module Agda.Interaction.ServerInteractionTop where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Text

data Request
  = LoadR   { rFilePath :: FilePath
            }

  | GoalsR
  | GoalR
  deriving (Eq, Show)

instance FromJSON Request where
  parseJSON (Object o)
    = o .: "type" >>= parseRequest o

  parseJSON _
    = mzero

parseRequest :: Object -> Text -> Parser Request
parseRequest o "load"
  = LoadR <$> o .: "filePath"

parseRequest o "goals"
  = pure GoalsR

parseRequest o "goal"
  = pure GoalR

parseRequest o _
  = mzero
