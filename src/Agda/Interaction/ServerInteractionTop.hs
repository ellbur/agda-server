{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Agda.Interaction.ServerInteractionTop where

import Agda.Interaction.InteractionTop
import Agda.Interaction.Response
import Agda.TypeChecking.Errors
import Agda.TypeChecking.Monad.Base

import Control.Applicative
import Control.Monad
import Data.Aeson hiding (json)
import Data.Aeson.Types
import Data.IORef
import Data.Text
import Web.Scotty

import qualified Control.Monad.State as St
import qualified Data.Foldable as F
import qualified Data.Sequence as S

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

  toJSON (Resp_Status (Status showImplicits checked))
    = typedObject "status"
        [ "showImplicitArguments" .= showImplicits
        , "checked" .= checked
        ]

  toJSON (Resp_DisplayInfo info)
    = displayInfoToJSON info

  toJSON Resp_ClearRunningInfo
    = typedObject "clearRunningInfo" []

  toJSON Resp_ClearHighlighting
    = typedObject "clearHighlighting" []

  toJSON _
    = object []

displayInfoToJSON :: DisplayInfo -> Value
displayInfoToJSON Info_CompilationOk
  = typedObject "compilationOk" []

displayInfoToJSON (Info_Constraints cs)
  = typedObject "constraints" ["value" .= cs]

displayInfoToJSON (Info_AllGoals gs)
  = typedObject "goals" ["value" .= gs]

displayInfoToJSON (Info_Error err)
  = typedObject "error" ["message" .= err]

displayInfoToJSON _
  = typedObject "display" []

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

runIOTCM :: CommandState -> IOTCM -> IO (S.Seq Response, CommandState)
runIOTCM st ioTCM
  = do
      responseRef <- newIORef S.empty

      result <- runTCM $ do
        setInteractionOutputCallback $ \res ->
          St.liftIO (modifyIORef responseRef (S.|> res))

        runCommandM (runInteraction ioTCM) st

      case result of
        Left err -> do
          errResponse <-
            Resp_DisplayInfo . Info_Error . either undefined id <$>
              runTCM (prettyError err)

          return (S.singleton errResponse, st)

        Right (_, st') ->
          (, st') <$> readIORef responseRef

setInteractionOutputCallback :: InteractionOutputCallback -> TCM ()
setInteractionOutputCallback f
  = St.modify $ \st -> st { stInteractionOutputCallback = f }

type Port
  = Int

runAgdaServer :: Port -> IO ()
runAgdaServer port
  = do
      stRef <- newIORef initCommandState

      scotty port $ do
        post "/" $ do
          ioTCM <- requestIOTCM <$> jsonData
          st <- St.liftIO (readIORef stRef)
          (rs, st') <- St.liftIO (runIOTCM st ioTCM)
          St.liftIO (writeIORef stRef st')
          let arr = toJSON (F.toList rs)
          json arr
