{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Mixpanel
( mixpanel
, defaultConfig
, MixpanelConfig(..)
, export
) where

import Network.Wreq
import Data.Aeson.Types
import Data.Aeson
import Data.Map.Strict as Map
import Data.Text as Text
import qualified Data.Text.Lazy as LazyText(lines)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.ByteString.Lazy (ByteString)
import Data.Time
import Data.Time.Clock.POSIX
import Data.Hash.MD5
import Control.Lens
import Control.Monad
import Network.HTTP.Client (defaultManagerSettings, managerResponseTimeout)


class MixpanelQuery a where
  toMap :: a -> Map Text Text
  endPoint :: a -> String
  endPoint a = "http://data.mixpanel.com/api/2.0/export/"
  
data MixpanelConfig = MixpanelConfig { apiSecret :: Text
                                     , apiKey :: Text
                                     , secondsToExpire :: Int
                                     } deriving (Show)

defaultConfig = MixpanelConfig { apiSecret = ""
                              , apiKey = ""
                              , secondsToExpire = 60
                              }

instance MixpanelQuery (Map Text Text) where
  toMap = Map.delete "endpoint"
  endPoint map = Text.unpack $ map ! "endpoint"

mixpanel :: (MixpanelQuery a, FromJSON b) => MixpanelConfig -> a -> IO (Maybe b)
mixpanel conf query = do
  response <- mixpanelRequest conf query $ endPoint query 
  let json = decode $ response  ^. responseBody 
  return json

export :: (MixpanelQuery a, FromJSON b) => MixpanelConfig -> a -> IO [Maybe b]
export conf query = do
  response <- mixpanelRequest conf query $ endPoint query
  return $ Prelude.map decode $ splitEvents $ response ^. responseBody
  -- This `splitEvents` function is needed because the export api doesn't really return json, it returns 
  -- a file of json objects separated by new lines. This means I need to decode the 
  -- bytestring so it can be broken into the separate objects to be parsed
  where splitEvents = Prelude.map encodeUtf8 . LazyText.lines . decodeUtf8 

parametersToString :: Map Text Text -> Text
parametersToString = foldlWithKey(\a key value -> Text.concat [a, key, "=", value]) ""

signature :: MixpanelConfig -> Map Text Text -> Text
signature conf params = Text.pack $ md5s $ Str $ Text.unpack $ parametersToString params `append` apiSecret conf

mixpanelRequest conf query url = do
  expireTime <- liftM ((+ secondsToExpire conf) . round) getPOSIXTime
  let key = apiKey conf
  let dict = insert "api_key" key
           $ insert "expire" (pack $ show expireTime)
           $ toMap query
  let token = signature conf dict 
  let opts = foldlWithKey(\a key value -> a & param key .~ [value]) defaults dict
                    & param "sig" .~ [token] 
                    & manager .~ Left (defaultManagerSettings { managerResponseTimeout = Just 3600000000} )
  getWith opts url
