{-# LANGUAGE OverloadedStrings #-}

import           Data.List
import           Servant.Elm
import           Data.Text hiding (intercalate, map)
import           Elm (toElmDecoderSource, toElmEncoderSource, toElmTypeSource)

import           Api

main :: IO ()
main = do
  let code = "module Api exposing (..)" :
            defElmImports :
            "type NoContent = NoContent" :
            toElmTypeSource (Proxy :: Proxy Item) :
            toElmDecoderSource (Proxy :: Proxy Item) :
            generateElmForAPI api
  writeFile "client/Api.elm" $ intercalate "\n\n" $ map unpack code
