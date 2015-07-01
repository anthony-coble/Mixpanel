{-# LANGUAGE OverloadedStrings #-}

module Mixpanel.Types
( Annotation(..)
) where

import Data.Text as Text
import Data.Aeson.Types
import Control.Applicative
import Data.Time
import Control.Monad
import System.Locale

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

