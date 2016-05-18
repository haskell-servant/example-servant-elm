
import           Network.Wai.Handler.Warp

import           App

main :: IO ()
main = run 3000 =<< app
