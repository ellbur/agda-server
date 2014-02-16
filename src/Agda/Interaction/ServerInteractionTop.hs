
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Agda.Interaction.ServerInteractionTop where

import Agda.Interaction.ServerInteractionProtocol

import Agda.Interaction.InteractionTop
import Agda.Interaction.BasicOps (Rewrite(..))
import Agda.Interaction.Response
import Agda.TypeChecking.Errors
import Agda.TypeChecking.Monad.Base

import Data.ByteString.Lazy.Char8 (unpack)

import Control.Applicative
import Control.Monad
import Data.Aeson hiding (json)
import Data.Aeson.Types
import Data.IORef
import Data.Text.Lazy (pack)
import Web.Scotty
import Data.Aeson.Encode.Pretty (encodePretty, encodePretty'')

import qualified Control.Monad.State as St
import qualified Data.Foldable as F
import qualified Data.Sequence as S

deriving instance Show Rewrite
deriving instance Show Interaction
deriving instance Show IOTCM

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
      putStrLn $ "See http://localhost:" ++ (show port) ++ "/ for documentation.\n"

      commandState <- newIORef initCommandState
      
      scotty port $ do
        get "/" $ do
            html $ pack (
                       "<html>\n"
                    ++ "<body>\n"
                    ++ "<p>POST to / to interact with the Server. Response body is JSON-encoded IOTCM.</p>\n"
                    ++ "\n"
                    ++ "<p>See <a href=\"/examples\">/examples</a> for syntax examples.</p>\n"
                    ++ "</body>\n"
                    ++ "</html>\n"
                )

        get "/examples" $ do
            let
                ex a b c d = "<pre>" ++ ( unpack (encodePretty (toJSON (IOTCM a b c d))) ) "</pre>\n"

            html $ pack (
                       "<html>\n"
                    ++ "<body>\n"
                    
                    ++ "<li>Loading a module:\n"
                    ++ ex "TheModule.agda" Interactive Direct $ Cmd_load "TheModule.agda" ["/include/dir/", "/another/include/dir"]
                    ++ "</li>\n"
                    
                    ++ "</body>\n"
                    ++ "</html>\n"
                )

        post "/" $ do
            (ioTCM :: IOTCM) <- jsonData
            St.liftIO $ print ioTCM
            commandStateBefore <- St.liftIO (readIORef commandState)
            (responses, commandStateAfter) <- St.liftIO (runIOTCM commandStateBefore ioTCM)
            St.liftIO (writeIORef commandState commandStateAfter)
            json $ toJSON responses


