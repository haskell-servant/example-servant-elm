
module AppSpec where

import           Control.Exception (throwIO, ErrorCall(..))
import           Control.Monad.Trans.Except
import           Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import           Network.HTTP.Types
import           Network.Wai.Handler.Warp
import           Servant.API
import           Servant.Client
import           Test.Hspec

import           Api
import           App (app)

getItemIds :: ClientM [ItemId]
getItem :: ItemId -> ClientM Item
postItem :: String -> ClientM ItemId
deleteItem :: ItemId -> ClientM NoContent
getItemIds :<|> getItem :<|> postItem :<|> deleteItem = client api

spec :: Spec
spec = do
  describe "app" $ around withApp $ do
    context "/api/item" $ do
      it "returns an empty list" $ \ host -> do
        try host getItemIds `shouldReturn` []

      context "/api/item/:id" $ do
        it "returns a 404 for missing items" $ \ (manager, baseUrl) -> do
          Left err <- runClientM (getItem 23) (ClientEnv manager baseUrl)
          responseStatus err `shouldBe` notFound404

      context "POST" $ do
        it "allows to create an item" $ \ host -> do
          i <- try host $ postItem "foo"
          try host (getItem i) `shouldReturn` Item i "foo"

        it "lists created items" $ \ host -> do
          i <- try host $ postItem "foo"
          try host getItemIds `shouldReturn` [i]

        it "lists 2 created items" $ \ host -> do
          a <- try host $ postItem "foo"
          b <- try host $ postItem "bar"
          try host getItemIds `shouldReturn` [a, b]

      context "DELETE" $ do
        it "allows to delete items" $ \ host -> do
          i <- try host $ postItem "foo"
          _ <- try host $ deleteItem i
          try host getItemIds `shouldReturn` []

type Host = (Manager, BaseUrl)

try :: Host -> ClientM a -> IO a
try (manager, baseUrl) action = do
  result <- runClientM action (ClientEnv manager baseUrl)
  case result of
    Right x -> return x
    Left err -> throwIO $ ErrorCall $ show err

withApp :: (Host -> IO a) -> IO a
withApp action = testWithApplication app $ \ port -> do
  manager <- newManager defaultManagerSettings
  let url = BaseUrl Http "localhost" port ""
  action (manager, url)
