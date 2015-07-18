{-# LANGUAGE OverloadedStrings #-}

module Mixpanel.Core
( defaultConfig
, mixpanelRequest
, MixpanelConfig(..)
) where

import Network.Wreq
import Data.Aeson.Types
import Data.Map.Strict as Map
import Data.Text as Text
import Data.Time
import Data.Time.Clock.POSIX
import Data.Hash.MD5
import Control.Lens
import Control.Monad
import Network.HTTP.Client (defaultManagerSettings, managerResponseTimeout)

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
  expireTime <- liftM ((+(secondsToExpire config)) . round) getPOSIXTime
  let key = apiKey config
  let dict = insert "api_key" key
           $ insert "expire" (pack $ show $ expireTime)
           args :: (Map Text Text)
  let paramString = foldlWithKey(\a key value -> Text.concat [a, key, "=", value]) "" dict
  let token = Text.pack $ md5s $ Str $ Text.unpack $ paramString `append` (apiSecret config)
  let opts = foldlWithKey(\a key value -> a & param key .~ [value]) defaults dict
                    & param "sig" .~ [token]
                    & manager .~ Left (defaultManagerSettings { managerResponseTimeout = Just 3600000000} ) 
  getWith opts url

