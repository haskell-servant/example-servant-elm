{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import           Data.Proxy
import           Servant.API
import qualified Elm.Derive
import           Elm.Module

type Api =
  "api" :>
    ("item" :> Get '[JSON] [ItemId] :<|>
     "item" :> Capture "itemId" ItemId :> Get '[JSON] Item :<|>
     "item" :> ReqBody '[JSON] String :> Post '[JSON] ItemId :<|>
     "item" :> Capture "itemId" ItemId :> Delete '[JSON] NoContent)

api :: Proxy Api
api = Proxy

-- types

type ItemId = Int

data Item
  = Item {
    id :: ItemId,
    text :: String
  }
  deriving (Show, Eq)

Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''Item
