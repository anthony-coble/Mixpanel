{-# LANGUAGE OverloadedStrings #-}

module Mixpanel.Annotations
( defaultConfig
, mixpanelRequest
, MixpanelConfig(..)
, Annotation(..)
, annotations
) where

import Data.Map.Strict as Map
import Data.Text as Text
import Data.Time
import Mixpanel.Core
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Lens
import Control.Applicative
import Control.Monad
import System.Locale 
import Data.Time.Format
import qualified Control.Lens as Lens
import Network.Wreq

data Annotation = Annotation { annotationId :: Int
                             , projectId :: Int
                             , date :: Day
                             , description :: Text
                             } deriving (Show, Read, Eq)

instance FromJSON Annotation where
 parseJSON (Object v) =
    Annotation <$> v .: "id"
               <*> v .: "project_id"
               <*> v .: "date"
               <*> v .: "description"
 parseJSON _ = mzero

instance ToJSON Annotation where
 toJSON a =
    object [ "id"          .= annotationId a
           , "project_id"  .= projectId a
           , "date"        .= date a
           , "description" .= description a
           ]

instance FromJSON Day where
 parseJSON (String a) = case (parseTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (Text.unpack a)) of
                        Just d -> pure d
                        _      -> fail "Couldn't parse date, expected format 'YYYY-MM-DD HH:MM:SS'"
 parseJSON v = typeMismatch "Day" v

instance ToJSON Day where
 toJSON a = 
  String (pack (formatTime defaultTimeLocale "%Y-%m-%d 00:00:00" a))

annotations config from to = do
  let fromString = Text.pack $ showGregorian from
  let toString = Text.pack $ showGregorian to
  let dict = insert "from_date" fromString
           $ insert "to_date" toString
             Map.empty
  r <- mixpanelRequest config dict "http://mixpanel.com/api/2.0/annotations/"
  let body = r Lens.^. responseBody
  -- print $ (decode body :: Maybe [Annotation])
  let ann = decode body
  return $ case (ann Lens.^. key "annotations" :: Maybe [Annotation]) of
           Just a -> a
           _      -> fail "Could not parse json response"
  -- return json
  -- return body

