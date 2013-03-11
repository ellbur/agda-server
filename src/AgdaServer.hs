module AgdaServer where

import Agda.Interaction.InteractionTop
import Agda.Interaction.ServerInteractionTop

import Data.IORef

main :: IO ()
main
  = do
      ref <- newIORef initCommandState



      print ()
