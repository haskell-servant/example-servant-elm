
import           Data.List
import           Servant.Elm

import           Api

main :: IO ()
main = do
  putStr $ intercalate "\n\n" $
    "module Api exposing (..)" :
    defElmImports :
    generateElmForAPI api
