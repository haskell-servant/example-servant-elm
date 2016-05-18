
import           Data.List
import           Servant.Elm

import           Api

main :: IO ()
main = do
  putStrLn "module Api exposing (..)"
  putStrLn defElmImports
  putStrLn $ intercalate "\n\n" $ generateElmForAPI api
