
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Agda.Interaction.ServerInteractionProtocol where

import Agda.Interaction.InteractionTop
import Agda.Interaction.BasicOps (Rewrite(..))
import Agda.Interaction.Response
import Agda.Interaction.FindFile (ModuleToSource)
import Agda.Interaction.Highlighting.Precise (HighlightingInfo, CompressedFile, MetaInfo, OtherAspect, Aspect, NameKind)
import qualified Agda.Interaction.Highlighting.Range as HighlightingRange
import Agda.TypeChecking.Errors
import Agda.TypeChecking.Monad.Base (InteractionId(..), HighlightingLevel, HighlightingMethod, RunMetaOccursCheck)
import Agda.Syntax.Position (Range'(..), Interval'(..), Position'(..))
import qualified Agda.Utils.Pretty as AgdaPretty
import Agda.Utils.FileName (AbsolutePath)

import Agda.Syntax.Concrete (Expr)
import Agda.Syntax.Concrete.Name (TopLevelModuleName)
import Agda.Syntax.Common (Induction)

import qualified Text.PrettyPrint as PrettyPrint

import Data.ByteString.Lazy.Char8 (unpack)

import Control.Applicative
import Control.Monad
import Data.Aeson hiding (json)
import Data.Aeson.Types
import Data.Aeson.TH
import Data.IORef

import qualified Data.Foldable as F
import qualified Data.Sequence as S
import qualified Data.Vector as Vector
import qualified Data.Map as Map

-- The request protocol is bidirectional simply so that you can produce examples of what
-- different kinds of requests should look like.

$(deriveJSON defaultOptions ''InteractionId)
    
$(deriveJSON defaultOptions ''AbsolutePath)
    
$(deriveJSON defaultOptions ''Position')
    
$(deriveJSON defaultOptions ''Interval')
    
$(deriveJSON defaultOptions ''Range')
    
$(deriveJSON defaultOptions ''Backend)
    
$(deriveJSON defaultOptions ''Rewrite)

$(deriveJSON defaultOptions ''HighlightingLevel)

$(deriveJSON defaultOptions ''HighlightingMethod)

$(deriveJSON defaultOptions ''Interaction)

$(deriveJSON defaultOptions ''IOTCM)

instance ToJSON Expr where
    toJSON x = toJSON $ AgdaPretty.pretty x

instance ToJSON PrettyPrint.Doc where
    toJSON x = toJSON $ PrettyPrint.render x

instance (ToJSON a) => ToJSON (S.Seq a) where
    toJSON x = Array $ Vector.fromList $ map toJSON (F.toList x)
    
instance ToJSON ModuleToSource where
    toJSON m2s = toJSON $ Map.toList m2s
    
$(deriveToJSON defaultOptions ''GiveResult)
    
$(deriveToJSON defaultOptions ''HighlightingRange.Range)
    
$(deriveToJSON defaultOptions ''Induction)
    
$(deriveToJSON defaultOptions ''NameKind)
    
$(deriveToJSON defaultOptions ''Aspect)
    
$(deriveToJSON defaultOptions ''OtherAspect)

$(deriveToJSON defaultOptions ''RunMetaOccursCheck)
    
$(deriveToJSON defaultOptions ''MetaInfo)

$(deriveToJSON defaultOptions ''CompressedFile)
    
$(deriveToJSON defaultOptions ''TopLevelModuleName)

$(deriveToJSON defaultOptions ''Status)
    
$(deriveToJSON defaultOptions ''DisplayInfo)

$(deriveToJSON defaultOptions ''Response)

