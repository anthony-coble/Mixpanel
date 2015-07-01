{-# LANGUAGE OverloadedStrings #-}

module Mixpanel.Requests
( defaultConfig
, mixpanelRequest
, MixpanelConfig(..)
, annotations
) where

import Data.Map.Strict as Map
import Data.Text as Text
import Data.Time
import Mixpanel.Types
import Mixpanel.Core
import Data.Aeson
import Data.Aeson.Lens
import Data.Time.Format
import Control.Lens
import Network.Wreq

-- | Query Mixpanel annotations
-- | MixpanelConfig for your project 
-- | Start Day
-- | End Day
annotations :: MixpanelConfig -> Day -> Day -> IO [Annotation]
annotations config from to = do
  let fromString = Text.pack $ showGregorian from
  let toString   = Text.pack $ showGregorian to
  let dict = insert "from_date" fromString
           $ insert "to_date" toString
             Map.empty
  r <- mixpanelRequest config dict "http://mixpanel.com/api/2.0/annotations/"
  let annotationsJson = decode $ r ^. responseBody
  return $ case (annotationsJson ^. key "annotations" :: Maybe [Annotation]) of
           Just a -> a
           _      -> fail "Could not parse json response"

