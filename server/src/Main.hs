
import           Network.Wai.Handler.Warp
import           System.IO

import           App

main :: IO ()
main = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr
          ("listening on port " ++ show port ++ "...")) $
        defaultSettings
  runSettings settings =<< app
