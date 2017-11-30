{-# LANGUAGE TypeOperators #-}

module App where

import           Control.Concurrent
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.Map
import           Network.Wai
import           Network.Wai.MakeAssets
import           Servant

import           Api

type WithAssets = Api :<|> Raw

withAssets :: Proxy WithAssets
withAssets = Proxy

app :: IO Application
app =
  serve withAssets <$> server

server :: IO (Server WithAssets)
server = do
  assets <- serveAssets
  db <- mkDB
  return $ apiServer db :<|> assets

apiServer :: DB -> Server Api
apiServer db =
  listItems db :<|>
  getItem db :<|>
  postItem db :<|>
  deleteItem db

listItems :: DB -> Handler [ItemId]
listItems db = liftIO $ allItemIds db

getItem :: DB -> ItemId -> Handler Item
getItem db n = maybe (throwE err404) return =<< liftIO (lookupItem db n)

postItem :: DB -> String -> Handler ItemId
postItem db new =
  liftIO $ insertItem db new

-- fake DB

data DB = DB (MVar (Map ItemId String))

debug :: DB -> IO ()
debug (DB mvar) = readMVar mvar >>= print

mkDB :: IO DB
mkDB = DB <$> newMVar empty

insertItem :: DB -> String -> IO ItemId
insertItem (DB mvar) new = modifyMVar mvar $ \ m -> do
  let newKey = case keys m of
        [] -> 0
        ks -> succ (maximum ks)
  return (insert newKey new m, newKey)

lookupItem :: DB -> ItemId -> IO (Maybe Item)
lookupItem (DB mvar) i = do
  fmap (Item i) <$> Data.Map.lookup i <$> readMVar mvar

allItemIds :: DB -> IO [ItemId]
allItemIds (DB mvar) =
  keys <$> readMVar mvar

deleteItem :: MonadIO m => DB -> ItemId -> m NoContent
deleteItem (DB mvar) i = liftIO $ do
  modifyMVar_ mvar $ \ m -> return (delete i m)
  return NoContent
