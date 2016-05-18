{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import           Data.Aeson
import           Data.Proxy
import           GHC.Generics
import           Servant.API
import           Servant.Elm

type Api =
  "api" :> Get '[JSON] TodoItem

api :: Proxy Api
api = Proxy

data TodoItem
  = TodoItem {
    id :: Integer,
    text :: String
  }
  deriving (Generic)

instance ElmType TodoItem
instance ToJSON TodoItem
