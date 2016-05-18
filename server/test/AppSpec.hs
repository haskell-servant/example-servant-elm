
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

type CHandler = ExceptT ServantError IO

getItemIds :: (BaseUrl, Manager) -> CHandler [ItemId]
getItemIds (baseUrl, manager) =
  let get :<|> _ = client api baseUrl manager
  in get

getItem :: (BaseUrl, Manager) -> ItemId -> CHandler Item
getItem (baseUrl, manager) =
  let _ :<|> get :<|> _ = client api baseUrl manager
  in get

postItem :: (BaseUrl, Manager) -> String -> CHandler ItemId
postItem (baseUrl, manager) =
  let _ :<|> _ :<|> get :<|> _ = client api baseUrl manager
  in get

deleteItem :: (BaseUrl, Manager) -> ItemId -> CHandler ItemId
deleteItem (baseUrl, manager) =
  let _ :<|> _ :<|> _ :<|> get = client api baseUrl manager
  in get

spec :: Spec
spec = do
  describe "app" $ around withApp $ do
    context "/api/item" $ do
      it "returns an empty list" $ \ c -> do
        try (getItemIds c) `shouldReturn` []

      context "/api/item/:id" $ do
        it "returns a 404 for missing items" $ \ c -> do
          Left err <- runExceptT (getItem c 23)
          responseStatus err `shouldBe` notFound404

      context "POST" $ do
        it "allows to create an item" $ \ c -> do
          i <- try $ postItem c "foo"
          try (getItem c i) `shouldReturn` Item i "foo"

        it "lists created items" $ \ c -> do
          i <- try $ postItem c "foo"
          try (getItemIds c) `shouldReturn` [i]

        it "lists 2 created items" $ \ c -> do
          a <- try $ postItem c "foo"
          b <- try $ postItem c "bar"
          try (getItemIds c) `shouldReturn` [a, b]

      context "DELETE" $ do
        it "allows to delete items" $ \ c -> do
          i <- try $ postItem c "foo"
          _ <- try $ deleteItem c i
          try (getItemIds c) `shouldReturn` []

try :: CHandler a -> IO a
try action = do
  result <- runExceptT action
  case result of
    Right x -> return x
    Left err -> throwIO $ ErrorCall $ show err

withApp :: ((BaseUrl, Manager) -> IO a) -> IO a
withApp action = testWithApplication app $ \ port -> do
  manager <- newManager defaultManagerSettings
  let url = BaseUrl Http "localhost" port ""
  action (url, manager)
