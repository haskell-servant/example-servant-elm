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
     "item" :> Capture "itemId" ItemId :> Delete '[JSON] Item)

-- TODO: Changed delete to return item. More adaptions necessary

api :: Proxy Api
api = Proxy

-- types

newtype ItemId = ItemdId Int
    deriving ( Show, Eq )

data Item
  = Item {
    id :: ItemId,
    text :: String
  }
  deriving (Show, Eq)

Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''Item
Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''ItemId
