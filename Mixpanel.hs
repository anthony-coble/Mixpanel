{-# LANGUAGE OverloadedStrings #-}

module Mixpanel 
( defaultConfig
, mixpanelRequest
) where

import Network.Wreq
import Data.Map.Strict as Map
import Data.Text as Text
import Data.Time
import Data.Hash.MD5
import Control.Lens

data MixpanelConfig = MixpanelConfig { apiSecret :: Text
                                     , apiKey :: Text
                                     , secondsToExpire :: Int 
                                     } deriving (Show)

defaultConfig = MixpanelConfig { apiSecret = ""
                              , apiKey = ""
                              , secondsToExpire = 60
                              }

-- | Make a request using the proper headers for mixpanel authentication
mixpanelRequest config args url = do
  time <- getCurrentTime
  let expireTime = (round $ utctDayTime time) + (secondsToExpire config)
  let key = apiKey config
  let dict = insert "api_key" key
           $ insert "expire" (pack $ show $ expireTime)
           args :: (Map Text Text)
  let paramString = foldlWithKey(\a key value -> Text.concat [a, key, "=", value]) "" dict 
  let token = Text.pack $ md5s $ Str $ Text.unpack $ paramString `append` (apiSecret config)

  let opts = foldlWithKey(\a key value -> a & param key .~ [value]) defaults dict
                    & param "sig" .~ [token]
  print token
  print expireTime
  print $ round $ utctDayTime time
  getWith opts url
