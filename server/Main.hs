{-# LANGUAGE TypeOperators #-}

import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.MakeAssets
import           Servant

import           Api

main :: IO ()
main = run 3000 =<< app

type WithAssets = Api :<|> Raw

withAssets :: Proxy WithAssets
withAssets = Proxy

app :: IO Application
app = serve withAssets <$> server

server :: IO (Server WithAssets)
server = do
  assets <- serveAssets
  return $ apiServer :<|> assets

apiServer :: Server Api
apiServer = return $ TodoItem 42 "hooray!!!"
