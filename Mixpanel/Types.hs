{-# LANGUAGE OverloadedStrings #-}

module Mixpanel.Types
( Annotation(..)
, Event(..)
) where

import Data.Text as Text
import Data.Aeson.Types
import Control.Applicative
import Data.Time
import Control.Monad
import System.Locale
import Data.Map.Strict as Map

data Annotation = Annotation { annotationId :: Int
                             , projectId :: Int
                             , date :: Day
                             , description :: Text
                             } deriving (Show, Read, Eq)

data Event = Event { name :: Text
                   , properties :: Map Text Text
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

instance FromJSON Event where
 parseJSON (Object v) = 
    Event <$> v .:? "event" .!= "Nil"
          <*> v .:? "kitten" .!= Map.empty
 parseJSON _ = mzero

-- Event reponse decoding: decode $ encodeUtf8 $ (Data.Text.Lazy.lines $ decodeUtf8 $ events ^. responseBody) !! 0 :: Maybe Value

instance ToJSON Event where
 toJSON e = 
  object [ "event" .= name e
         , "properties" .= properties e
         ]    

instance FromJSON Day where
 parseJSON (String a) = case (parseTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (Text.unpack a)) of
                        Just d -> pure d
                        _      -> fail "Couldn't parse date, expected format 'YYYY-MM-DD HH:MM:SS'"
 parseJSON v = typeMismatch "Day" v

instance ToJSON Day where
 toJSON a =
  String (pack (formatTime defaultTimeLocale "%Y-%m-%d 00:00:00" a))

