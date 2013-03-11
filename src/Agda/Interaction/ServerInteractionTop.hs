{-# LANGUAGE OverloadedStrings #-}

module Agda.Interaction.ServerInteractionTop where

import Agda.Interaction.InteractionTop
import Agda.Interaction.Response
import Agda.TypeChecking.Monad.Base

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Text
import Web.Scotty

data Request
  = RequestR FilePath RequestType
  deriving (Eq, Show)

data RequestType
  = LoadR
  | GoalsR
  | GoalR
  deriving (Eq, Show)

instance FromJSON Request where
  parseJSON (Object o)
    = do
        ty <- o .: "type"
        filePath <- o .: "filePath"

        RequestR filePath <$>
          parseRequestType ty filePath o

  parseJSON _
    = mzero

parseRequestType :: String -> FilePath -> Object -> Parser RequestType
parseRequestType "load" filePath o
  = pure LoadR

parseRequestType "goals" filePath o
  = pure GoalsR

parseRequestType "goal" filePath o
  = pure GoalR

parseRequestType ty filePath o
  = mzero

typedObject :: Text -> [Pair] -> Value
typedObject ty pairs
  = object (("type" .= ty) : pairs)

instance ToJSON Response where
  toJSON (Resp_HighlightingInfo info mod)
    = typedObject "highlighting" []

  toJSON _
    = object []

requestInteraction :: Request -> Interaction
requestInteraction (RequestR filePath LoadR)
  = Cmd_load filePath []

requestInteraction (RequestR filePath GoalsR)
  = Cmd_metas

requestInteraction (RequestR filePath GoalR)
  = Cmd_metas

requestIOTCM :: Request -> IOTCM
requestIOTCM rq@(RequestR filePath ty)
  = IOTCM filePath NonInteractive Direct $
      requestInteraction rq

runIOTCM :: CommandState -> IOTCM -> IO CommandState
runIOTCM st ioTCM
  = either (const st) snd <$>
      runTCM (runCommandM (runInteraction ioTCM) st)
