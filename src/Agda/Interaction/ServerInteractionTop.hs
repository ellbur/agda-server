{-# LANGUAGE OverloadedStrings #-}

module Agda.Interaction.ServerInteractionTop where

import Agda.Interaction.InteractionTop
import Agda.Interaction.Response

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Text
import Web.Scotty

data Request
  = LoadR FilePath
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

typedObject :: Text -> [Pair] -> Value
typedObject ty pairs
  = object (("type" .= ty) : pairs)

instance ToJSON Response where
  toJSON (Resp_HighlightingInfo info mod)
    = typedObject "highlighting" []

  toJSON _
    = object []

interpret :: Request -> IO Response
interpret (LoadR filePath)
  = undefined

interpret GoalsR
  = undefined

interpret GoalR
  = undefined
