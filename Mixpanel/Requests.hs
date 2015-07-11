{-# LANGUAGE OverloadedStrings #-}

module Mixpanel.Requests
( defaultConfig
, mixpanelRequest
, MixpanelConfig(..)
, annotations
, export
) where

import qualified Data.Map.Strict as Map (insert, empty, Map(..))
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText(lines)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Data.Time
import Mixpanel.Types
import Mixpanel.Core
import Data.Aeson
import Data.Aeson.Lens
import Data.Time.Format
import Control.Lens
import Network.Wreq
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Maybe (mapMaybe)

-- | Query Mixpanel annotations
-- | MixpanelConfig for your project 
-- | Start Day
-- | End Day
annotations :: MixpanelConfig -> Day -> Day -> IO [Annotation]
annotations config from to = do
  let fromString = Text.pack $ showGregorian from
  let toString   = Text.pack $ showGregorian to
  let dict = Map.insert "from_date" fromString
           $ Map.insert "to_date" toString
             Map.empty
  r <- mixpanelRequest config dict "http://mixpanel.com/api/2.0/annotations/"
  let annotationsJson = decode $ r ^. responseBody
  return $ case (annotationsJson ^. key "annotations" :: Maybe [Annotation]) of
           Just a -> a
           _      -> fail "Could not parse json response"

export config to from events = do
  let fromString = Text.pack $ showGregorian from
  let toString   = Text.pack $ showGregorian to
  let dict = Map.insert "from_date" fromString
           $ Map.insert "to_date" toString
           $ Map.insert "event" (Text.pack $ unpack $ encode $ events)
           $ Map.empty -- :: Map Text.Text Text.Text
  r <- mixpanelRequest config dict "http://data.mixpanel.com/api/2.0/export/"
  -- return $ mapMaybe (decode) $ splitEvents $ r ^. responseBody
  return $ splitEvents $ r ^. responseBody
  -- This `splitEvents` function is needed because the export api doesn't really return json, it returns 
  -- a file of json objects separated by new lines. This means I need to decode the 
  -- bytestring so it can be broken into the separate objects to be parsed
  where splitEvents = map (encodeUtf8) . LazyText.lines . decodeUtf8 
